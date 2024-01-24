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
    elif isinstance(expr, UnevalSubst):
        yield from preOrderTraversal(expr.expr)
        yield expr.old
        yield expr.new
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


def replaceVar(expr: Expr, old: Var, new: Var) -> None:
    if old == new:
        return

    def replChild(expr: Expr, child: str):
        var = getattr(expr, child)
        if isinstance(var, Var) and var == old:
            setattr(expr, child, new)
        else:
            replaceVar(getattr(expr, child), old, new)

    if isinstance(expr, Var):
        return
    elif isinstance(expr, Abstr):
        replChild(expr, "var")
        replChild(expr, "expr")
    elif isinstance(expr, App):
        replChild(expr, "expr")
        replChild(expr, "arg")
    elif isinstance(expr, UnevalSubst):
        replChild(expr, "old")
        replChild(expr, "new")
        replChild(expr, "expr")
    else:
        assert False


# Replace by dictionary {old: new}
def replaceVarDict(expr: Expr, dic: Dict[Var, Var]) -> None:
    def replChild(expr: Expr, child: str):
        var = getattr(expr, child)
        if isinstance(var, Var) and var in dic:
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
    elif isinstance(expr, UnevalSubst):
        replChild(expr, "old")
        replChild(expr, "new")
        replChild(expr, "expr")
    else:
        assert False
