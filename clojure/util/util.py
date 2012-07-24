

def walk(child_selector, map_func, tree):
    """Walks a tree. Children are specified via child_selector
       applies map_func to each item in the tree"""
    map_func(tree)
    for x in child_selector(tree):
        walk(child_selector, map_func, x)

def flatten(child_selector, tree):
    """Flattens a tree"""
    yield tree
    for x in child_selector(tree):
        for y in x:
            yield y

def partial(func, arg):
    """partially apply arg to func"""
    def partial_inner(*args):
        return func(arg, *args)

    return partial_inner
