from typing import Callable, Dict, Sequence, Generator, Set

from .Lambda import *


def currying(varList: Sequence[Var], expr: Expr) -> Expr:
    if len(varList) == 0:
        return expr
    elif len(varList) == 1:
        return Abstr(varList[0], expr)
    else:
        cur = Abstr(varList[-1], expr)
        for var in reversed(varList[:-1]):
            cur = Abstr(var, cur)
        return cur


def preOrderTraversal(expr: Expr) -> Generator[Expr, None, None]:
    yield expr
    if isinstance(expr, Var):
        yield expr
    elif isinstance(expr, Abstr):
        yield expr.var
        yield from preOrderTraversal(expr.expr)
    elif isinstance(expr, App):
        yield from preOrderTraversal(expr.expr)
        yield from preOrderTraversal(expr.arg)
    elif isinstance(expr, Add):
        yield from preOrderTraversal(expr.left)
        yield from preOrderTraversal(expr.right)
    elif isinstance(expr, MaxN):
        for arg in expr.args:
            yield from preOrderTraversal(arg)
    else:
        assert False


def getAllVars(expr: Expr) -> Set[Var]:
    varSet = set()
    for var in preOrderTraversal(expr):
        if isinstance(var, Var):
            varSet.add(var)
    return varSet


def getAllVarsIf(expr: Expr, pred: Callable[[Var], bool]) -> Set[Var]:
    varSet = set()
    for var in preOrderTraversal(expr):
        if isinstance(var, Var) and pred(var):
            varSet.add(var)
    return varSet


# Replace by dictionary {old: new}
def replaceVarDict(expr: Expr, dic: Dict[Var, Var]) -> None:
    def replChild(expr: Expr, child: str):
        var = getattr(expr, child)
        if isinstance(var, Var):
            if var in dic:
                setattr(expr, child, dic[var])
        else:
            replaceVarDict(getattr(expr, child), dic)

    if isinstance(expr, Var):
        return
    elif isinstance(expr, Abstr):
        replChild(expr, "var")
        replChild(expr, "expr")
    elif isinstance(expr, App):
        replChild(expr, "expr")
        replChild(expr, "arg")
    elif isinstance(expr, Add):
        replChild(expr, "left")
        replChild(expr, "right")
    elif isinstance(expr, MaxN):
        for i, arg in enumerate(expr.args):
            var = expr.args[i]
            if isinstance(var, Var):
                if var in dic:
                    expr.args[i] = dic[var]
            else:
                replaceVarDict(arg, dic)
    else:
        assert False
