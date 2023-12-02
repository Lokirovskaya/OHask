from typing import List
from sympy.utilities.iterables import iterable
from ...struct import Constraint, MyLambda
from ...dep.preprocess.SymbolMaker import isComplFunc, isVar, isLit, isParam
from .Target import Target
from ...Log import log


def findTargets(constrList: List[Constraint]) -> List[Target]:
    result = []

    for constr in constrList:
        unknownExprs = findUnknownExprs(constr.rhs)
        result += [Target(expr=expr, belongs=constr) for expr in unknownExprs]

    log("[Expressions For Dynamic Analysis]")
    for target in result:
        log(target)
    log()

    return result


# Traversal on every T_f(expr)
# If the expr is not a trivial var or lit, then it is an unknown expr
def findUnknownExprs(expr):
    result = set()

    def isTrivial(s):
        return isParam(s) or isVar(s) or isLit(s) or isinstance(s, MyLambda)

    def runExpr(expr):
        if isComplFunc(expr):
            for arg in expr.args:
                if not isTrivial(arg):
                    result.add(arg)
            return  # no nesting T_f func

        if not iterable(expr.args):
            return
        for arg in expr.args:
            runExpr(arg)

    runExpr(expr)
    return result
