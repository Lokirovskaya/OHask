from typing import List

import sympy

from calcComplexity.Log import logln
from calcComplexity.constraint import Constraint, SympyConstraint
from calcComplexity.solve.util.HigherOrder import HigherSymbol
import calcComplexity.untypedLambdaCalculus as lam

from .util.ZEncoder import zEncode


def convertToSympy(constrList: List[Constraint]) -> List[SympyConstraint]:
    result = []

    def makeSymbol(var):
        return sympy.Symbol(zEncode(var.name))

    for constr in constrList:
        lhs = makeSymbol(constr.lhs)
        params = list(map(makeSymbol, constr.lhsParams))
        rhs = convertExpr(constr.rhs)
        sympyConstr = SympyConstraint(lhs, params, rhs)
        result.append(sympyConstr)

    logln("[SymPy Constraints]")
    for constr in result:
        logln(str(constr))
    logln()

    return result


def convertExpr(expr: lam.Expr):
    if isinstance(expr, lam.Var):
        return convertVar(expr)
    elif isinstance(expr, lam.Abstr):
        return convertAbstr(expr)
    elif isinstance(expr, lam.App):
        return convertApp(expr)
    elif isinstance(expr, lam.Sum):
        return convertSum(expr)
    elif isinstance(expr, lam.MaxN):
        return convertMaxN(expr)
    else:
        assert False


def convertVar(var: lam.Var):
    return HigherSymbol(zEncode(var.name), var.arity)


def convertAbstr(abstr: lam.Abstr):
    var = convertVar(abstr.var)
    expr = convertExpr(abstr.expr)
    return sympy.Lambda(var, expr)


def convertApp(app: lam.App):
    expr = convertExpr(app.expr)
    arg = convertExpr(app.arg)
    return expr.rcall(arg)


def convertSum(sum_: lam.Sum):
    result = convertExpr(sum_.args[0])
    for arg in sum_.args[1:]:
        result += convertExpr(arg)
    return result


def convertMaxN(maxN: lam.MaxN):
    symArgs = map(convertExpr, maxN.args)
    return sympy.Function("maxN")(*symArgs)
