from typing import List

import sympy

from calcComplexity.Log import logln
from calcComplexity.constraint import Constraint, SympyConstraint
from calcComplexity.solve.util.SympySymbols import makeSymbol
import calcComplexity.untypedLambdaCalculus as lam

from .util.ZEncoder import zEncode


def convertToSympy(constrList: List[Constraint]) -> List[SympyConstraint]:
    result = []

    for constr in constrList:
        lhsRaw = makeSymbol(constr.lhs.name, arity=len(constr.lhsParams))
        lhsParams = map(lambda v: makeSymbol(v.name), constr.lhsParams)
        lhs = lhsRaw(*lhsParams)
        rhs = convertExpr(constr.rhs)
        sympyConstr = SympyConstraint(lhs, rhs)
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
    return makeSymbol(var.name, var.arity)


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
