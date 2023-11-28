from typing import List
from ..struct.Constraint import Constraint
from ..struct.MyLambda import MyLambda
from sympy import Symbol, Function
from sympy.core import postorder_traversal
from sympy.core.function import UndefinedFunction
from sympy.utilities.iterables import iterable


# Complexity terms which are not depends on any function params are effectively constant.
def removeConstTerms(constrList: List[Constraint]):
    for constr in constrList:
        if isinstance(constr.rhs, MyLambda):
            params = list(constr.rhs.signature)
            expr = constr.rhs.expr
        else:
            constr.rhs = 1
            return

        constTerms = findAllConstTerms(expr, params)
        repDict = {t: 1 for t in constTerms}
        constr.rhs = constr.rhs.xreplace(repDict)


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


def isComplFunc(s):
    return isinstance(s, (UndefinedFunction, Function)) and s.name.startswith("T")


def isParam(s):
    return isinstance(s, (Symbol, UndefinedFunction, Function)) and s.name.startswith(
        "p"
    )
