#!/usr/bin/env python
"""Main entry point to clojure-py: sets import hook, import clojure.core and
define main().
"""

import cPickle
import imp
from optparse import OptionParser
import os.path
import sys
import traceback

from clojure.lang.cljexceptions import NoNamespaceException
from clojure.lang.compiler import Compiler
from clojure.lang.fileseq import StringReader
from clojure.lang.globals import currentCompiler
from clojure.lang.lispreader import read
from clojure.lang.namespace import Namespace, findItem
import clojure.lang.rt as RT
from clojure.lang.symbol import Symbol
from clojure.lang.var import threadBindings


VERSION = "0.2.4"
VERSION_MSG = "clojure-py {0}\nPython {1}".format(VERSION, sys.version)


class MetaImporter(object):
    """A PEP302 import hook for clj files.
    """

    def find_module(self, fullname, path=None):
        """Finds a clj file if there is no package with the same name.
        """
        lastname = fullname.rsplit(".", 1)[-1]
        for d in path or sys.path:
            clj = os.path.join(d, lastname + ".clj")
            pkg = os.path.join(d, lastname, "__init__.py")
            pkgc = getattr(imp, "cache_from_source",
                           lambda path: path + "c")(pkg)
            if (os.path.exists(clj) and
                not (os.path.exists(pkg) or os.path.exists(pkgc))):
                self.path = clj
                return self
        return None

    def load_module(self, name):
        """Loads a clj file, returns the corresponding namespace if it exists.
        
        If the file did not create the corresponding namespace,
        NoNamespaceException (a subclass of ImportError) is raised.
        """
        if name not in sys.modules:
            sys.modules[name] = None # avoids circular imports
            try:
                requireClj(self.path)
            except Exception as exc:
                del sys.modules[name]
                traceback.print_exc()
                raise ImportError("requireClj raised an exception.")
            if sys.modules[name] == None:
                del sys.modules[name]
                raise NoNamespaceException(self.path, name)
            sys.modules[name].__loader__ = self
        return sys.modules[name]

    def is_package(self, name):
        return False


sys.meta_path.append(MetaImporter())


def requireClj(filename, stopafter=None):
    """Compiles and executes the code in a clj file.
    
    If `stopafter` is given, then stop execution as soon as the `stopafter`
    name is defined in the current namespace of the compiler.
    """
    with open(filename) as fl:
        r = StringReader(fl.read())

    RT.init()
    comp = Compiler()
    comp.setFile(filename)

    with threadBindings({currentCompiler: comp}): #, open(filename + ".cljc", "w") as o:
        try:
            while True:
                EOF = object()
                s = read(r, False, EOF, True)
                if s is EOF:
                    break
                #cPickle.dump(s, o)
                try:
                    res = comp.compile(s)
                    comp.executeCode(res)
                    if stopafter is not None and hasattr(comp.getNS(), stopafter):
                        break
                except Exception as exp:
                    print s, filename
                    raise
        except IOError as e:
            pass


def main():
    """Main entry point to clojure-py.
    """

    def gobble(option, opt_str, value, parser):
        """Interprets all the remaining arguments as a single argument.
        """
        setattr(parser.values, option.dest, " ".join(parser.rargs))
        del parser.rargs[:]

    parser = OptionParser(
        usage="%prog [options] ... [-c cmd | file | -] [arg] ...",
        version=VERSION_MSG)
    parser.add_option("-c",
        action="callback", dest="cmd", default="", callback=gobble,
        help="program passed in as a string (terminates option list)")
    parser.add_option("-i", action="store_true", dest="interactive",
        help="inspect interactively after running script")
    parser.add_option("-q", action="store_true", dest="quiet",
        help="don't print version message on interactive startup")
    # fooling OptionParser
    parser.add_option("--\b\bfile", action="store_true",
        help="    program read from script file")
    parser.add_option("--\b\b-", action="store_true",
        help="    program read from stdin (default; interactive mode if a tty)")
    parser.add_option("--\b\barg ...", action="store_true",
        help="    arguments passed to program in *command-line-args*")
    args = sys.argv[1:]
    try:
        i = args.index("-")
    except ValueError:
        i = len(args)
    dash_and_post = args[i:]
    opts, command_line_args = parser.parse_args(args[:i])
    source = command_line_args.pop(0) if command_line_args else None
    command_line_args.extend(dash_and_post)
    opts.command_line_args = command_line_args

    RT.init()
    comp = Compiler()

    command_line_args_sym = findItem(Namespace("clojure.core"),
                                     Symbol("*command-line-args*"))
    with threadBindings({currentCompiler: comp,
                         command_line_args_sym: command_line_args}):
        if source:
            requireClj(source)
        if opts.interactive or not source and not opts.cmd:
            import clojure.repl
            clojure.repl.enable_readline()
            clojure.repl.run_repl(opts, comp)


if __name__ == "__main__":
    main()
