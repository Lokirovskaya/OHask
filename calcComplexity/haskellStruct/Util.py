from typing import Generator, Set
from .Struct import *


def preOrderTraversal(expr: Expr) -> Generator[Expr, None, None]:
    yield expr
    if isinstance(expr, Var) or isinstance(expr, Lit):
        yield expr
    elif isinstance(expr, App):
        yield from preOrderTraversal(expr.appExpr)
        yield from preOrderTraversal(expr.appArg)
    elif isinstance(expr, Case):
        yield from preOrderTraversal(expr.caseExpr)
        for alt in expr.caseAlts:
            yield from preOrderTraversal(alt.altExpr)
    else:
        assert False


def getAllVars(expr: Expr) -> Set[Var]:
    varSet = set()
    for var in preOrderTraversal(expr):
        if isinstance(var, Var):
            varSet.add(var)
    return varSet


# Tackle with type constructor (,) (,,) ...
# They are not real identifier, so a lot of workaround is required
def isTupleConstructorName(varName: str) -> bool:
    if len(varName) == 3:
        return varName == "(,)"
    else:
        return (
            len(varName) >= 3
            and varName[0] == "("
            and varName[-1] == ")"
            and all(map(lambda c: c == ",", varName[1:-1]))
        )


builtinVars = [":", "[]", "+", "-", "*"]


def isBuiltinVar(var: Var) -> bool:
    return var.varName in builtinVars or isTupleConstructorName(var.varName)
