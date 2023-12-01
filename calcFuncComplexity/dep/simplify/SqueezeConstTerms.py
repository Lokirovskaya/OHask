from typing import List
from ...struct import Constraint, MyLambda
from ..preprocess.SymbolMaker import isComplFunc, isParam
from sympy import Symbol, Function
from sympy.core import postorder_traversal
from sympy.core.function import UndefinedFunction
from sympy.utilities.iterables import iterable


# Complexity terms which are not depends on any function params are effectively constant.
def squeezeConstTerms(constrList: List[Constraint]):
    for constr in constrList:
        if isinstance(constr.rhs, MyLambda):
            params = list(constr.rhs.signature)
            expr = constr.rhs.expr
        else:
            assert False, f"Bad RHS type {type(constr.rhs)} in {constr.rhs}"
            return

        constTerms = findAllConstTerms(expr, params)
        for t in constTerms:
            constr.substitute(t, 1)


# Traversal on expr tree, find all T_f(args) where args fits (not isDependsOnParams(args))
# Ensure there is no nesting T_f calls
def findAllConstTerms(expr, params):
    result = set()

    def runExpr(expr, params):
        if isComplFunc(expr) and not isDependsOnParams(expr, params):
            result.add(expr)

        if not iterable(expr.args):
            return
        for arg in expr.args:
            runExpr(arg, params)

    runExpr(expr, params)
    return result


def isDependsOnParams(expr, params):
    for expr in postorder_traversal(expr):
        if isParam(expr):
            if expr in params:
                return True
    return False


