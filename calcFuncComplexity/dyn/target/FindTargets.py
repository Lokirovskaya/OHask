from typing import List
from sympy.utilities.iterables import iterable
from ...struct import Constraint
from ...dep.preprocess.SymbolMaker import isComplFunc, isVar, isLit
from .Target import Target


def findTargets(constrList: List[Constraint]) -> List[Target]:
    result = []
    
    for constr in constrList:
        unknownExprs = findUnknownExprs(constr.rhs)
        result += [Target(expr=expr, belongs=constr) for expr in unknownExprs]
        
    return result


# Traversal on every T_f(expr)
# If the expr is not a trivial var or lit, then it is an unknown expr
def findUnknownExprs(expr):
    result = set()

    def runExpr(expr):
        if isComplFunc(expr) and not isVar(expr) and not isLit(expr):
            result.update(expr.args)
            return  # no nesting T_f func

        if not iterable(expr.args):
            return
        for arg in expr.args:
            runExpr(arg)

    runExpr(expr)
    return result
