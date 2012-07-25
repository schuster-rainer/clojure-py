import __builtin__
import dis
import marshal
import pickle
import py_compile
import re
import sys
import time
import fractions


from clojure.util.treadle import treadle as tr
from clojure.util.util import *

from clojure.lang.cons import Cons
from clojure.lang.cljexceptions import CompilerException, AbstractMethodCall
from clojure.lang.cljkeyword import Keyword, keyword
from clojure.lang.ipersistentvector import IPersistentVector
from clojure.lang.ipersistentmap import IPersistentMap
from clojure.lang.ipersistentset import IPersistentSet
from clojure.lang.ipersistentlist import IPersistentList
from clojure.lang.iseq import ISeq
from clojure.lang.lispreader import _AMP_, LINE_KEY, garg
from clojure.lang.namespace import (findItem,
                                    find as findNamespace,
                                    findOrCreate as findOrCreateNamespace)
from clojure.lang.persistentlist import PersistentList, EmptyList
from clojure.lang.persistentvector import PersistentVector
import clojure.lang.rt as RT
from clojure.lang.symbol import Symbol, symbol
from clojure.lang.var import (
    Var, define, intern as internVar, var as createVar,
    pushThreadBindings, popThreadBindings)
import marshal
import types
import copy

ConstNone = tr.Const(None)

_MACRO_ = keyword(symbol("macro"))
_NS_ = symbol("*ns*")
version = (sys.version_info[0] * 10) + sys.version_info[1]

PTR_MODE_GLOBAL = "PTR_MODE_GLOBAL"
PTR_MODE_DEREF = "PTR_MODE_DEREF"

AUDIT_CONSTS = False

class ResolutionContext(object):
    def __init__(self, comp):
        self.comp = comp

    def __enter__(self):
        self.aliases = self.comp.aliases
        self.recurPoint = self.comp.recurPoint
        self.comp.aliases = copy.copy(self.comp.aliases)
        self.comp.recurPoint = copy.copy(self.comp.recurPoint)

    def __exit__(self, type, value, traceback):
        self.comp.aliases = self.aliases
        self.comp.recurPoint = self.recurPoint

class Quoted(object):
    def __init__(self, comp):
        self.comp = comp

    def __enter__(self):
        self.comp.inQuote = True

    def __exit__(self, type, value, traceback):
        self.comp.inQuote = False

class MetaBytecode(object):
    pass


class GlobalPtr(tr.AExpression):
    def __init__(self, ns, name):
        self.ns = ns
        self.name = name

    def size(self, current, max_seen):
        # +3 just to be safe
        return current + 1, max(max_seen, current + 3)

    def __repr__(self):
        return "GblPtr<%s/%s>" % (self.ns.__name__, self.name)

    def emit(self, ctx):
        module = self.ns
        val = getattr(module, self.name)

        expr = tr.Call(getAttrChain(self.ns + "."+ self.name))
        if isinstance(val, Var):
            return tr.Call(tr.Attr(expr, "deref"))

        return expr

def maybeDeref(ns, nsname, sym, curmodulename):
    val = getattr(ns, sym)

    if curmodulename == nsname:
        expr = tr.Global(sym)
    else:
        expr = getAttrChain(nsname.getName() + "."+ sym)

    if isinstance(val, Var):
        return tr.Call(tr.Attr(expr, "deref"))

    return expr


def expandMetas(bc, comp):
    code = []
    for x in bc:
        if AUDIT_CONSTS and isinstance(x, tuple):
            if x[0] == LOAD_CONST:
                try:
                    marshal.dumps(x[1])
                except:
                    print "Can't marshal", x[1], type(x[1])
                    raise

        if isinstance(x, MetaBytecode):
            code.extend(x.emit(comp, PTR_MODE_DEREF))
        else:
            code.append(x)
    return code


def emitJump(label):
    if version == 26:
        return [(JUMP_IF_FALSE, label),
                (POP_TOP, None)]
    else:
        return [(POP_JUMP_IF_FALSE, label)]


def emitLanding(label):
    if version == 26:
        return [(label, None),
                (POP_TOP, None)]
    else:
        return [(label, None)]


builtins = {}

def register_builtin(sym):
    """
    A decorator to register a new builtin macro. Pass the symbol that the macro
    represents as the argument. If the argument is a string, it will be
    converted to a symbol.
    """
    def inner(func):
        builtins[sym if isinstance(sym, Symbol) else symbol(sym)] = func
        return func
    return inner


@register_builtin("in-ns")
def compileNS(comp, form):
    rest = form.next()
    if len(rest) != 1:
        raise CompilerException("ns only supports one item", rest)
    ns = rest.first()
    if not isinstance(ns, Symbol):
        ns = comp.executeCode(comp.compile(ns))
    comp.setNS(ns)
    return tr.Const(None)


@register_builtin("def")
def compileDef(comp, form):
    if len(form) not in [2, 3]:
        raise CompilerException("Only 2 or 3 arguments allowed to def", form)
    sym = form.next().first()
    value = None
    if len(form) == 3:
        value = form.next().next().first()
    if sym.ns is None:
        ns = comp.getNS()
    else:
        ns = sym.ns

    comp.pushName(RT.name(sym))
    code = []
    i = getAttrChain("clojure.lang.var.intern")
    v = tr.Call(i, tr.Const(comp.getNS().__name__), tr.Const(sym.getName()))

    # We just wrote some treadle code to define a var, but
    # the compiler won't see that yet, so let's re-define the var now
    # so that the compiler can pick it up

    internVar(comp.getNS().__name__, sym.getName())

    #v.setDynamic(True)
    if len(form) == 3:
        code = tr.Call(tr.Attr(v, "bindRoot"),
                           comp.compile(value))


    else:
        code = v

    with Quoted(comp):
        code = tr.Call(tr.Attr(code, "setMeta"), comp.compile(sym.meta()))
    #v.setMeta(sym.meta())
    comp.popName()
    return code


def compileBytecode(comp, form):
    codename = form.first().name
    if not hasattr(tr, codename):
        raise CompilerException("bytecode {0} unknown".format(codename), form)
    bc = getattr(tr, codename)

    form = form.next()
    arg = None

    s = form
    code = []
    while s is not None:
        code.append(comp.compile(s.first()))
        s = s.next()

    try:
        code = bc(*code)
    except:
        print "error compiling "

    return code


@register_builtin("kwapply")
def compileKWApply(comp, form):
    if len(form) < 3:
        raise CompilerException("at least two arguments required to kwapply", form)

    form = form.next()
    fn = form.first()
    form = form.next()
    kws = form.first()
    args = form.next()
    code = []

    s = args
    code.extend(comp.compile(fn))
    while s is not None:
        code.extend(comp.compile(s.first()))
        s = s.next()
    code.extend(comp.compile(kws))
    code.append((LOAD_ATTR, "toDict"))
    code.append((CALL_FUNCTION, 0))
    code.append((CALL_FUNCTION_KW, 0 if args is None else len(args)))
    return code

@register_builtin("loop*")
def compileLoopStar(comp, form):
    if len(form) < 3:
        raise CompilerException("loop* takes at least two args", form)
    form = form.next()
    if not isinstance(form.first(), PersistentVector):
        raise CompilerException(
            "loop* takes a vector as it's first argument", form)
    s = form.first()
    args = []
    code = []
    idx = 0
    while idx < len(s):
        if len(s) - idx < 2:
            raise CompilerException(
                "loop* takes a even number of bindings", form)
        local = s[idx]
        if not isinstance(local, Symbol) or local.ns is not None:
            raise CompilerException(
                "bindings must be non-namespaced symbols", form)

        idx += 1

        body = s[idx]
        if local in comp.aliases:
            newlocal = symbol("{0}_{1}".format(local, RT.nextID()))
            code.extend(comp.compile(body))
            comp.pushAlias(local, RenamedLocal(newlocal))
            args.append(local)
        else:
            comp.pushAlias(local, RenamedLocal(local))
            args.append(local)
            code.extend(comp.compile(body))

        code.extend(comp.getAlias(local).compileSet(comp))
        idx += 1

    form = form.next()
    recurlabel = Label("recurLabel")
    recur = {"label": recurlabel,
             "args": map(lambda x: comp.getAlias(x).compileSet(comp), args)}
    code.append((recurlabel, None))
    comp.pushRecur(recur)
    code.extend(compileImplcitDo(comp, form))
    comp.popRecur()
    comp.popAliases(args)
    return code


@register_builtin("let*")
def compileLetStar(comp, form):
    if len(form) < 3:
        raise CompilerException("let* takes at least two args", form)
    form = form.next()
    if not isinstance(form.first(), IPersistentVector):
        raise CompilerException(
            "let* takes a vector as it's first argument", form)
    s = form.first()
    args = []
    code = []
    idx = 0

    with ResolutionContext(comp):
        code = []
        while idx < len(s):
            if len(s) - idx < 2:
                raise CompilerException(
                    "let* takes a even number of bindings", form)
            local = s[idx]
            if not isinstance(local, Symbol) or local.ns is not None:
                raise CompilerException(
                    "bindings must be non-namespaced symbols", form)

            idx += 1

            body = s[idx]

            if comp.getAlias(local) is not None:
                cb = (comp.compile(body))
                newlocal = symbol("{0}_{1}".format(local, RT.nextID()))
                comp.pushAlias(local, tr.Local(newlocal.getName()))
                args.append(local)
            else:
                cb = comp.compile(body)
                comp.pushAlias(local, tr.Local(local.getName()))
                args.append(local)

            code.append(tr.StoreLocal(comp.getAlias(local), cb))

            idx += 1

        form = form.next()
        code.append(compileImplcitDo(comp, form))
        code = tr.Do(*code)

        return code


@register_builtin(".")
def compileDot(comp, form):
    if len(form) != 3:
        raise CompilerException(". form must have two arguments", form)
    clss = form.next().first()
    member = form.next().next().first()

    if isinstance(member, Symbol):
        attr = member.name
        args = []
    elif isinstance(member, ISeq):
        if not isinstance(member.first(), Symbol):
            raise CompilerException("Member name must be symbol", form)
        attr = member.first().name
        args = []
        if len(member) > 1:
            f = member.next()
            while f is not None:
                args.append(comp.compile(f.first()))
                f = f.next()

    alias = comp.getAlias(clss)
    if alias:
        code = alias.compile(comp)
        code = tr.Attr(code, attr)
    else:
        code = comp.compile(symbol(clss, attr))

    code = tr.Call(code, *args)
    return code


@register_builtin("quote")
def compileQuote(comp, form):
    with Quoted(comp):
        if len(form) != 2:
            raise CompilerException("Quote must only have one argument", form)

        return comp.compile(form.next().first())


@register_builtin(symbol("py", "if"))
def compilePyIf(comp, form):
    if len(form) != 3 and len(form) != 4:
        raise CompilerException("if takes 2 or 3 args", form)
    cmp = comp.compile(form.next().first())
    body = comp.compile(form.next().next().first())
    if len(form) == 3:
        body2 = [(LOAD_CONST, None)]
    else:
        body2 = comp.compile(form.next().next().next().first())

    return tr.If(cmp, body, body2)


@register_builtin("if*")
def compileIfStar(comp, form):
    """
    Compiles the form (if* pred val else?).
    """
    if len(form) != 3 and len(form) != 4:
        raise CompilerException("if takes 2 or 3 args", form)
    cmp = comp.compile(form.next().first())
    body = comp.compile(form.next().next().first())
    if len(form) == 3:
        body2 = tr.ConstNone
    else:
        body2 = comp.compile(form.next().next().next().first())

    elseLabel = Label("IfElse")
    endlabel = Label("IfEnd")
    condresult = tr.Local(garg(0).name)

    code = tr.Do(tr.StoreLocal(cmp),
                 tr.If(tr.And(IsNot(condresult, tr.ConstNone),
                              IsNot(condresult, tr.ConstFalse)),
                             body,
                             body2))

    return code


def unpackArgs(form):
    locals = {}
    args = []
    lastisargs = False
    argsname = None
    for x in form:
        if x == _AMP_:
            lastisargs = True
            continue
        if lastisargs and argsname is not None:
            raise CompilerException(
                "variable length argument must be the last in the function",
                form)
        if lastisargs:
            argsname = x
        if not isinstance(x, Symbol) or x.ns is not None:
            raise CompilerException(
                "fn* arguments must be non namespaced symbols, got {0} instead".
                format(form), form)
        locals[x] = RT.list(x)
        args.append(x.name)
    return locals, args, lastisargs, argsname


@register_builtin("do")
def compileDo(comp, form):
    return compileImplcitDo(comp, form.next())


def resolveTrLocal(aliases, local):
    s = symbol(local)
    if s not in aliases:
        raise Exception("Could not resolve closure " + local)
    return aliases[s]

def compileFn(comp, name, form, orgform):
    locals, args, lastisargs, argsname = unpackArgs(form.first())

    with ResolutionContext(comp):
        trargs = []
        for x in args:
            if lastisargs and x == args[-1]:
                arg = tr.RestArgument(x)
            else:
                arg = tr.Argument(x)
            comp.pushAlias(symbol(x), arg)
            trargs.append(arg)

        resolved = partial(resolveTrLocal, comp.aliases)

        if lastisargs:
            expr = tr.Do(cleanRest(comp.getAlias(argsname)),
                         compileImplcitDo(comp, form.next()))
        else:
            expr = compileImplcitDo(comp, form.next())

        return tr.Func(trargs, expr, resolved)


def cleanRest(local):
    return local.StoreLocal(getAttrChain("__builtin__.len")
                                    .Call(local)
                                    .Equal(tr.Const(0))
                                    .If(tr.Const(None), local))





class MultiFn(object):
    def __init__(self, comp, form, argsv):
        with ResolutionContext(comp):
            form = RT.seq(form)
            if len(form) < 1:
                raise CompilerException("FN defs must have at least one arg", form)
            argv = form.first()
            if not isinstance(argv, PersistentVector):
                raise CompilerException("FN arg list must be a vector", form)
            body = form.next()

            self.locals, self.args, self.lastisargs, self.argsname = unpackArgs(argv)

            wlocals = map(lambda x: tr.Argument(x.getName()), self.locals)
            
            argcode = tr.GreaterOrEqual(
                tr.Call(getAttrChain("__builtin__.len"), argsv),
                tr.Const(len(self.args) - (1 if self.lastisargs else 0)))
            argscode = []
            for x in range(len(wlocals)):
                if self.lastisargs and x == len(wlocals) - 1:
                    offset = len(wlocals) - 1

                    argscode.append(tr.Do(wlocals[x].StoreLocal(argsv.Slice1(tr.Const(x))),
                        wlocals[x].StoreLocal(cleanRest(wlocals[x]))))

                else:
                    argscode.append(wlocals[x].StoreLocal(argsv.Subscript(tr.Const(x))))
                        

            for x in wlocals:
                comp.pushAlias(symbol(x.name),x)


            bodycode = compileImplcitDo(comp, body).Return()
            self.argcode = argcode
            self.extractcode = tr.Do(*argscode)
            self.bodycode = bodycode


def compileMultiFn(comp, name, form):
    s = form
    argdefs = []
    argsv = tr.RestArgument("__argsv__")
    while s is not None:
        argdefs.append(MultiFn(comp, s.first(), argsv))
        s = s.next()
    argdefs = sorted(argdefs, lambda x, y: len(x.args) < len(y.args))
    if len(filter(lambda x: x.lastisargs, argdefs)) > 1:
        raise CompilerException(
            "Only one function overload may have variable number of arguments",
            form)

    code = []
    if len(argdefs) == 1 and not argdefs[0].lastisargs:
        hasvararg = False
        argslist = argdefs[0].args
        code.append(tr.Do(argdefs[0].argcode, argdefs[0].bodycode))
    else:
        hasvararg = True
        for x in argdefs:
            code.append(tr.If(x.argcode, tr.Do(x.extractcode, x.bodycode)))

        code.append(tr.Global("Exception")
                      .Call(tr.Const("Bad Arity")
                      .Raise()))


    return tr.Func([argsv], tr.Do(*code))


def compileImplcitDo(comp, form):
    def cmpl(f):
        return comp.compile(f)

    return tr.Do(*map(cmpl, form))


@register_builtin("fn*")
def compileFNStar(comp, form):
    with ResolutionContext(comp):
        if len(comp.aliases) > 0: # we might have closures to deal with
            for x in comp.aliases:
                comp.pushAlias(x, tr.Closure(comp.getAlias(x).name, comp.getAlias(x)))

        orgform = form
        if len(form) < 2:
            raise CompilerException("2 or more arguments to fn* required", form)
        form = form.next()
        name = form.first()
        pushed = False

        if not isinstance(name, Symbol):
            name = comp.getNamesString() + "_auto_"
        else:
            comp.pushName(name.name)
            pushed = True
            form = form.next()

        name = symbol(name)

        # This is fun stuff here. The idea is that we want closures to be able
        # to call themselves. But we can't get a pointer to a closure until after
        # it's created, which is when we actually run this code. So, we're going to
        # create a tmp local that is None at first, then pass that in as a possible
        # closure cell. Then after we create the closure with MAKE_CLOSURE we'll
        # populate this var with the correct value


        # form = ([x] x)
        if isinstance(form.first(), IPersistentVector):
            expr = compileFn(comp, name, form, orgform)
        # form = (([x] x))
        elif len(form) == 1:
            expr = compileFn(comp, name, RT.list(*form.first()), orgform)
        # form = (([x] x) ([x y] x))
        else:
            expr = compileMultiFn(comp, name, form)

        if pushed:
            comp.popName()

        return expr


def compileVector(comp, form):
    code = []
    for x in form:
        code.append(comp.compile(x))
    code = tr.Call(getAttrChain("clojure.lang.rt.vector"), *code)
    return code


@register_builtin("recur")
def compileRecur(comp, form):
    return tr.Recur(*map(comp.compile, form.next()))



@register_builtin("is?")
def compileIs(comp, form):
    if len(form) != 3:
        raise CompilerException("is? requires 2 arguments", form)
    fst = form.next().first()
    itm = form.next().next().first()
    code = tr.Is(comp.compile(fst), comp.compile(itm))
    return code


def compileMap(comp, form):
    s = form.seq()
    code = []
    while s is not None:
        kvp = s.first()
        code.append(comp.compile(kvp.getKey()))
        code.append(comp.compile(kvp.getValue()))
        s = s.next()

    code = tr.Call(getAttrChain("clojure.lang.rt.map"), *code)
    return code


def compileKeyword(comp, kw):
    return tr.Call(getAttrChain("clojure.lang.cljkeyword.keyword"),
                   tr.Const(kw.getNamespace()),
                   tr.Const(kw.getName()))


def compileBool(comp, b):
    return tr.Const(b)


@register_builtin("throw")
def compileThrow(comp, form):
    if len(form) != 2:
        raise CompilerException("throw requires two arguments", form)
    code = comp.compile(form.next().first())
    code.append((RAISE_VARARGS, 1))
    return code


@register_builtin("applyTo")
def compileApply(comp, form):
    s = form.next()
    code = []
    while s is not None:
        code.extend(comp.compile(s.first()))

        s = s.next()
    code.append((LOAD_CONST, RT.seqToTuple))
    code.append((ROT_TWO, None))
    code.append((CALL_FUNCTION, 1))
    code.append((CALL_FUNCTION_VAR, 0))
    return code


def compileBuiltin(comp, form):
    if len(form) != 2:
        raise CompilerException("throw requires two arguments", form)
    name = str(form.next().first())
    return getBuiltin(name)


def getBuiltin(name):
    if hasattr(__builtin__, name):
        return tr.Attr(tr.Global("__builtin__"), name)
    raise CompilerException("Python builtin {0} not found".format(name), name)


@register_builtin("let-macro")
def compileLetMacro(comp, form):
    if len(form) < 3:
        raise CompilerException(
            "alias-properties takes at least two args", form)
    form = form.next()
    s = RT.seq(form.first())
    syms = []
    while s is not None:
        sym = s.first()
        syms.append(sym)
        s = s.next()
        if s is None:
            raise CompilerException(
                "let-macro takes a even number of bindings", form)
        macro = s.first()
        comp.pushAlias(sym, LocalMacro(sym, macro))
        s = s.next()
    body = form.next()
    code = compileImplcitDo(comp, body)

    return code


@register_builtin("__compiler__")
def compileCompiler(comp, form):
    return [(LOAD_CONST, comp)]


@register_builtin("try")
def compileTry(comp, form):
    """
    Compiles the try macro.
    """
    assert form.first() == symbol("try")
    form = form.next()

    if not form:
        # I don't like this, but (try) == nil
        return [(LOAD_CONST, None)]

    # Extract the thing that may raise exceptions
    body = form.first()

    form = form.next()
    if not form:
        # If there are no catch/finally/else etc statements, just
        # compile the budy
        return comp.compile(body)

    catch = []
    els = None
    fin = None
    for subform in form:
        # FIXME, could also be a Cons, LazySeq, etc.
        #if not isinstance(subform, IPersistentList):
        #    raise CompilerException("try arguments must be lists", form)
        if not len(subform):
            raise CompilerException("try arguments must not be empty", form)
        name = subform.first()
        if name in (symbol("catch"), symbol("except")):
            if len(subform) != 4:
                raise CompilerException(
                    "try {0} blocks must be 4 items long".format(name), form)

            # Exception is second, val is third
            exception = subform.next().first()
            if not isinstance(exception, Symbol):
                raise CompilerException(
                    "exception passed to {0} block must be a symbol".
                    format(name), form)
            for ex, _, _ in catch:
                if ex == exception:
                    raise CompilerException(
                        "try cannot catch duplicate exceptions", form)

            var = subform.next().next().first()
            if not isinstance(var, Symbol):
                raise CompilerException(
                    "variable name for {0} block must be a symbol".
                    format(name), form)
            val = subform.next().next().next().first()
            catch.append((exception, var, val))
        elif name == symbol("else"):
            if len(subform) != 2:
                raise CompilerException(
                    "try else blocks must be 2 items", form)
            elif els:
                raise CompilerException(
                    "try cannot have multiple els blocks", form)
            els = subform.next().first()
        elif name == symbol("finally"):
            if len(subform) != 2:
                raise CompilerException(
                    "try finally blocks must be 2 items", form)
            elif fin:
                raise CompilerException(
                    "try cannot have multiple finally blocks", form)
            fin = subform.next().first()
        else:
            raise CompilerException(
                "try does not accept any symbols apart from "
                "catch/except/else/finally, got {0}".format(form), form)

    if fin and not catch and not els:
        return compileTryFinally(comp.compile(body), comp.compile(fin))
    elif catch and not fin and not els:
        return compileTryCatch(comp, comp.compile(body), catch)
    elif not fin and not catch and els:
        raise CompilerException(
            "try does not accept else statements on their own", form)

    if fin and catch and not els:
        return compileTryCatchFinally(comp, comp.compile(body), catch,
                                      comp.compile(fin))

def compileTryFinally(body, fin):
    """
    Compiles the try/finally form. Takes the body of the try statement, and the
    finally statement. They must be compiled bytecode (i.e. comp.compile(body)).
    """
    finallyLabel = Label("TryFinally")

    ret_val = "__ret_val_{0}".format(RT.nextID())

    code = [(SETUP_FINALLY, finallyLabel)]
    code.extend(body)
    code.append((STORE_FAST, ret_val))
    code.append((POP_BLOCK, None))
    code.append((LOAD_CONST, None))
    code.append((finallyLabel, None))
    code.extend(fin)
    code.extend([(POP_TOP, None),
                 (END_FINALLY, None),
                 (LOAD_FAST, ret_val)])
    return code


def compileTryCatch(comp, body, catches):
    """
    Compiles the try/catch/catch... form. Takes the body of the try statement,
    and a list of (exception, exception_var, except_body) tuples for each
    exception. The order of the list is important.
    """
    assert len(catches), "Calling compileTryCatch with empty catches list"

    catch_labels = [Label("TryCatch_{0}".format(ex)) for ex, _, _ in catches]
    endLabel = Label("TryCatchEnd")
    endFinallyLabel = Label("TryCatchEndFinally")
    firstExceptLabel = Label("TryFirstExcept")

    ret_val = "__ret_val_{0}".format(RT.nextID())

    code = [(SETUP_EXCEPT, firstExceptLabel)] # First catch label
    code.extend(body)
    code.append((STORE_FAST, ret_val)) # Because I give up with
    # keeping track of what's in the stack
    code.append((POP_BLOCK, None))
    code.append((JUMP_FORWARD, endLabel)) # if all went fine, goto end

    n = len(catches)
    for i, (exception, var, val) in enumerate(catches):

        comp.pushAlias(var, FnArgument(var)) # FnArgument will do

        last = i == n - 1

        # except Exception
        code.extend(emitLanding(catch_labels[i]))
        if i == 0:
            # first time only
            code.append((firstExceptLabel, None))
        code.append((DUP_TOP, None))
        code.extend(comp.compile(exception))
        code.append((COMPARE_OP, "exception match"))
        code.extend(emitJump(catch_labels[i + 1] if not last else
                             endFinallyLabel))

        # as e
        code.append((POP_TOP, None))
        code.append((STORE_FAST, var.name))
        code.append((POP_TOP, None))

        # body
        code.extend(comp.compile(val))
        code.append((STORE_FAST, ret_val))
        code.append((JUMP_FORWARD, endLabel))

        comp.popAlias(var)

    code.extend(emitLanding(endFinallyLabel))
    code.append((END_FINALLY, None))
    code.append((endLabel, None))
    code.append((LOAD_FAST, ret_val))

    return code

def compileTryCatchFinally(comp, body, catches, fin):
    """
    Compiles the try/catch/finally form.
    """
    assert len(catches), "Calling compileTryCatch with empty catches list"

    catch_labels = [Label("TryCatch_{0}".format(ex)) for ex, _, _ in catches]
    finallyLabel = Label("TryCatchFinally")
    notCaughtLabel = Label("TryCatchFinally2")
    firstExceptLabel = Label("TryFirstExcept")
    normalEndLabel = Label("NoExceptionLabel")

    ret_val = "__ret_val_{0}".format(RT.nextID())

    code = [
        (SETUP_FINALLY, finallyLabel),
        (SETUP_EXCEPT, firstExceptLabel)] # First catch label
    code.extend(body)
    code.append((STORE_FAST, ret_val)) # Because I give up with
    # keeping track of what's in the stack
    code.append((POP_BLOCK, None))
    code.append((JUMP_FORWARD, normalEndLabel))
    # if all went fine, goto finally

    n = len(catches)
    for i, (exception, var, val) in enumerate(catches):

        comp.pushAlias(var, FnArgument(var)) # FnArgument will do

        last = i == n - 1
        first = i == 0

        # except Exception
        code.extend(emitLanding(catch_labels[i]))
        if first:
            # After the emitLanding, so as to split the label
            code.append((firstExceptLabel, None))
        code.append((DUP_TOP, None))
        code.extend(comp.compile(exception))
        code.append((COMPARE_OP, "exception match"))
        code.extend(emitJump(catch_labels[i + 1] if not last
                             else notCaughtLabel))

        # as e
        code.append((POP_TOP, None))
        code.append((STORE_FAST, var.name))
        code.append((POP_TOP, None))

        # body
        code.extend(comp.compile(val))
        code.append((STORE_FAST, ret_val))
        code.append((JUMP_FORWARD, normalEndLabel))

        comp.popAlias(var)

    code.extend(emitLanding(notCaughtLabel))
    code.append((END_FINALLY, None))
    code.append((normalEndLabel, None))
    code.append((POP_BLOCK, None))
    code.append((LOAD_CONST, None))

    code.append((finallyLabel, None))
    code.extend(fin)
    code.append((POP_TOP, None))
    code.append((END_FINALLY, None))
    code.append((LOAD_FAST, ret_val))

    return code


def compileList(comp, form):
    args = map(comp.compile, form)

    return tr.Call(tr.Global("list"), *args)


def getAttrChain(s):
    if isinstance(s, Symbol):
        s = s.getNamespace() + "." + s.getName()
    if isinstance(s, str):
        s = s.split(".")

    x = tr.Global(s[0])
    for c in s[1:]:
        x = tr.Attr(x, c)

    return x



"""
We should mention a few words about aliases. Aliases are created when the
user uses closures, fns, loop, let, or let-macro. For some forms like
let or loop, the alias just creates a new local variable in which to store the
data. In other cases, closures are created. To handle all these cases, we have
a base AAlias class which provides basic single-linked list abilites. This will
allow us to override what certain symbols resolve to.

For instance:

(fn bar [a b]
    (let [b (inc b)
          z 1]
        (let-macro [a (fn [fdecl& env& decl] 'z)]
            (let [o (fn [a] a)]
                 [a o b]))))

As each new local is created, it is pushed onto the stack, then only the
top most local is executed whenever a new local is resolved. This allows
the above example to resolve exactly as desired. lets will never stop on
top of eachother, let-macros can turn 'x into (.-x self), etc.
"""

class AAlias(object):
    """Base class for all aliases"""
    def __init__(self, rest = None):
        self.rest = rest
    def compile(self, comp):
        raise AbstractMethodCall(self)
    def compileSet(self, comp):
        raise AbstractMethodCall(self)
    def next(self):
        return self.rest


class FnArgument(AAlias):
    """An alias provided by the arguments to a fn*
       in the fragment (fn [a] a) a is a FnArgument"""
    def __init__(self, sym, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
    def compile(self, comp):
        return [(LOAD_FAST, RT.name(self.sym))]
    def compileSet(self, comp):
        return


class RenamedLocal(AAlias):
    """An alias created by a let, loop, etc."""
    def __init__(self, sym, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
        self.newsym = symbol(RT.name(sym) + str(RT.nextID()))
    def compile(self, comp):
        return [(LOAD_FAST, RT.name(self.newsym))]
    def compileSet(self, comp):
        return [(STORE_FAST, RT.name(self.newsym))]


class LocalMacro(AAlias):
    """represents a value that represents a local macro"""
    def __init__(self, sym, macroform, rest = None):
        AAlias.__init__(self, rest)
        self.sym = sym
        self.macroform = macroform
    def compile(self, comp):
        code = comp.compile(self.macroform)
        return code


class SelfReference(AAlias):
    def __init__(self, var, rest = None):
        AAlias.__init__(self, rest)
        self.var = var
        self.isused = False
    def compile(self, comp):
        self.isused = True
        return [(LOAD_CONST, self.var),
                (LOAD_ATTR, "deref"),
                (CALL_FUNCTION, 0)]


class Name(object):
    """Slot for a name"""
    def __init__(self, name, rest=None):
        self.name = name
        self.isused = False
        self.rest = rest

    def __str__(self):
        v = []
        r = self
        while r is not None:
            v.append(r.name)
            r = r.rest
        v.reverse()
        s = "_".join(v)
        if self.isused:
            s = s + str(RT.nextID())
        return s


def evalForm(form, ns):
    comp = Compiler()
    comp.setNS(ns)
    code = comp.compile(form)
    code = expandMetas(code, comp)
    return comp.executeCode(code)


def ismacro(macro):
    return (not isinstance(macro, type)
            and (hasattr(macro, "meta")
            and macro.meta()
            and macro.meta()[_MACRO_])
            or getattr(macro, "macro?", False))


def meta(form):
    return getattr(form, "meta", lambda: None)()


def macroexpand(form, comp, one = False):
    if isinstance(form.first(), Symbol):
        if form.first().ns == 'py' or form.first().ns == "py.bytecode":
            return form, False

        itm = findItem(comp.getNS(), form.first())
        dreffed = itm
        if isinstance(dreffed, Var):
            dreffed = itm.deref()

        # Handle macros here
        # TODO: Break this out into a seperate function
        if ismacro(itm) or ismacro(dreffed):
            macro = dreffed
            args = RT.seqToTuple(form.next())

            macroform = getattr(macro, "_macro-form", macro)

            mresult = macro(macroform, None, *args)

            if hasattr(mresult, "withMeta") and hasattr(form, "meta"):
                mresult = mresult.withMeta(form.meta())
            mresult = comp.compile(mresult)
            return mresult, True

    return form, False


class Compiler(object):
    def __init__(self):
        self.recurPoint = RT.list()
        self.names = None
        self.ns = None
        self.lastlineno = -1
        self.aliases = {}
        self.filename = "<unknown>"
        self.inQuote = False

    def setFile(self, filename):
        self.filename = filename

    def pushAlias(self, sym, alias):
        """ Sets the alias for the given symbol """
        self.aliases[sym] = alias

    def getAlias(self, sym):
        """ Retreives to top alias for this entry """
        if sym in self.aliases:
            return self.aliases[sym]
        return None

    def pushRecur(self, label):
        """ Pushes a new recursion label. All recur calls will loop back to this point """
        self.recurPoint = RT.cons(label, self.recurPoint)
    def popRecur(self):
        """ Pops the top most recursion point """
        self.recurPoint = self.recurPoint.next()

    def pushName(self, name):
        if self.names is None:
            self.names = Name(name)
        else:
            self.names = Name(name, self.names)

    def popName(self):
        self.names = self.names.rest

    def getNamesString(self, markused=True):
        if self.names is None:
            return "fn_{0}".format(RT.nextID())
        s = str(self.names)
        if markused and self.names is not None:
            self.names.isused = True
        return s

    def compileMethodAccess(self, form):
        attrname = form.first().name[1:]
        if len(form) < 2:
            raise CompilerException(
                "Method access must have at least one argument", form)
        c = self.compile(form.next().first())
        c = tr.Attr(c, attrname)
        s = form.next().next()
        args = []
        while s is not None:
            args.append(self.compile(s.first()))
            s = s.next()
        c = tr.Call(c, *args)
        return c

    def compilePropertyAccess(self, form):
        attrname = form.first().name[2:]
        if len(form) != 2:
            raise CompilerException(
                "Property access must have at only one argument", form)
        c = self.compile(form.next().first())
        c.append((LOAD_ATTR, attrname))
        return c

    def compileForm(self, form):
        if self.inQuote:
            return compileList(self, form)

        if form.first() in builtins:
            return builtins[form.first()](self, form)
        form, ret = macroexpand(form, self)
        if ret:
            return form
        if isinstance(form.first(), Symbol):
            if form.first().ns == "py.treadle":
                return compileBytecode(self, form)
            if form.first().name.startswith(".-"):
                return self.compilePropertyAccess(form)
            if form.first().name.startswith(".") and form.first().ns is None:
                return self.compileMethodAccess(form)
        c = [self.compile(form.first())]
        f = form.next()
        acount = 0
        while f is not None:
            c.append(self.compile(f.first()))
            acount += 1
            f = f.next()
        c = tr.Call(*c)

        return c

    def compileAccessList(self, sym):
        if sym.ns == 'py':
            return getBuiltin(RT.name(sym))

        code = self.getAccessCode(sym)
        return code

    def getAccessCode(self, sym):
        print sym
        if (sym.ns is not None and sym.ns == self.nsString) \
           or sym.ns is None:
            if self.getNS() is None:
                raise CompilerException("no namespace has been defined", None)
            if not hasattr(self.getNS(), RT.name(sym)):
                raise CompilerException(
                    "could not resolve '{0}', '{1}' not found in {2} reference {3}".
                    format(sym, RT.name(sym), self.getNS().__name__,
                           self.getNamesString(False)),
                    None)
            var = getattr(self.getNS(), RT.name(sym))

            return maybeDeref(self.ns, self.nsString, RT.name(sym), self.nsString)

        if symbol(sym.ns) in getattr(self.getNS(), "__aliases__", {}):
            sym = symbol(self.getNS().__aliases__[symbol(sym.ns)].__name__, RT.name(sym))

        splt = []
        if sym.ns is not None:
            module = findNamespace(sym.ns)
            if not hasattr(module, RT.name(sym)):
                raise CompilerException(
                    "{0} does not define {1}".format(module, RT.name(sym)),
                    None)

            return getAttrChain(sym.getNamespace() + "." + sym.getName())

        code = LOAD_ATTR if sym.ns else LOAD_GLOBAL
        #if not sym.ns and RT.name(sym).find(".") != -1 and RT.name(sym) != "..":
        raise CompilerException(
            "unqualified dotted forms not supported: {0}".format(sym), sym)

        if len(RT.name(sym).replace(".", "")):
            splt.extend((code, attr) for attr in RT.name(sym).split("."))
        else:
            splt.append((code, RT.name(sym)))
        return splt

    def compileSymbol(self, sym):
        """ Compiles the symbol. First the compiler tries to compile it
            as an alias, then as a global """
        if self.inQuote:
            return tr.Call(getAttrChain("clojure.lang.symbol.symbol"),
                        tr.Const(sym.getNamespace()),
                        tr.Const(sym.getName()))


        if sym in self.aliases:
            return self.aliases[sym]

        return self.compileAccessList(sym)

    def compileAlias(self, sym):
        """ Compiles the given symbol as an alias."""
        alias = self.getAlias(sym)
        if alias is None:
            raise CompilerException("Unknown Local {0}".format(sym), None)
        return alias.compile(self)

    def compile(self, itm):
        try:
            c = []
            lineset = False
            #if getattr(itm, "meta", lambda: None)() is not None:
            #    line = itm.meta()[LINE_KEY]
            #    if line is not None and line > self.lastlineno:
            #        lineset = True
            #        self.lastlineno = line
            #        c.append([SetLineno, line])

            if isinstance(itm, Symbol):
                return self.compileSymbol(itm)
            elif isinstance(itm, PersistentList) or isinstance(itm, Cons):
                return self.compileForm(itm)
            elif itm is None:
                return self.compileNone(itm)
            elif type(itm) in [str, int, types.ClassType, type, Var]:
                return tr.Const(itm)
            elif isinstance(itm, IPersistentVector):
                return compileVector(self, itm)
            elif isinstance(itm, IPersistentMap):
                return compileMap(self, itm)
            elif isinstance(itm, Keyword):
                return compileKeyword(self, itm)
            elif isinstance(itm, bool):
                return compileBool(self, itm)
            elif isinstance(itm, EmptyList):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, unicode):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, float):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, long):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, fractions.Fraction):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, IPersistentSet):
                c.append((LOAD_CONST, itm))
            elif isinstance(itm, type(re.compile(""))):
                c.append((LOAD_CONST, itm))
            else:
                raise CompilerException(
                    " don't know how to compile {0}".format(type(itm)), None)

            if len(c) < 2 and lineset:
                return []
            return c
        except:
            print "Compiling {0}".format(itm)
            raise


    def compileNone(self, itm):
        return tr.Const(None)

    def setNS(self, ns):
        self.nsString = ns
        self.ns = findOrCreateNamespace(ns)

    def getNS(self):
        if self.ns is not None:
            return self.ns

    def executeCode(self, code):

        ns = self.getNS()
        if code == []:
            return None
        print(code)


        pushThreadBindings(
            {findItem(findOrCreateNamespace("clojure.core"), _NS_): ns})
        retval = code.toFunc(ns.__dict__)()
        self.getNS().__file__ = self.filename
        popThreadBindings()
        return retval

    def pushPropertyAlias(self, mappings):
        locals = {}
        for x in mappings:
            if x in self.aliasedProperties:
                self.aliasedProperties[x].append(mappings[x])
            else:
                self.aliasedProperties[x] = [mappings[x]]

    def popPropertyAlias(self, mappings):
        dellist = []
        for x in mappings:
            self.aliasedProperties[x].pop()
            if not len(self.aliasedProperties[x]):
                dellist.append(x)
        for x in dellist:
            del self.aliasedProperties[x]

    def standardImports(self):
        return [(LOAD_CONST, -1),
            (LOAD_CONST, None),
            (IMPORT_NAME, "clojure.standardimports"),
            (IMPORT_STAR, None)]

    def executeModule(self, code):
        code.append((RETURN_VALUE, None))
        #c = Code(code, [], [], False, False, False, str(symbol(self.getNS().__name__, "<string>")), self.filename, 0, None)

        dis.dis(c)
        codeobject = c.to_code()

        with open('output.pyc', 'wb') as fc:
            fc.write(py_compile.MAGIC)
            py_compile.wr_long(fc, long(time.time()))
            marshal.dump(c, fc)
