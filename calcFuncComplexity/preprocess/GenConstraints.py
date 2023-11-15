from typing import Any, Dict, List, Callable
from ..struct.Constraint import Constraint
from ..struct.MaxCompl import MaxCompl
from .SymbolMaker import (
    makeComplSymbol,
    makeLambdaParamSymbol,
    makeParamSymbol,
    makeVarSymbol,
)
from .Api import Func, Expr, Var, App, Case
from sympy import Function, Symbol, Lambda, symbols


# List of equations
# [(lhs, rhs), ...]
def genConstraintList(funcList: List[Func]) -> List[Constraint]:
    constraintList = []

    for func in funcList:
        paramsSymbol = [
            makeParamSymbol(func.funcName, i) for i in range(len(func.funcParams))
        ]
        complSymbol = makeComplSymbol(func.funcName, paramsSymbol)
        complValue = calcFuncCompl(func)
        constraintList.append(Constraint(complSymbol, complValue))  # type: ignore

    return constraintList


def calcFuncCompl(func: Func):
    compl = calcExprCompl(func.funcExpr, func)
    return compl


def calcExprCompl(expr: Expr, curFunc: Func):
    if var := expr.matchVar():
        compl = calcVarCompl(var)
    elif expr.matchLit():
        compl = 0
    elif app := expr.matchApp():
        compl = calcAppCompl(app, curFunc)
    elif case_ := expr.matchCase():
        compl = calcCaseCompl(case_, curFunc)
    elif expr.matchLam():
        assert False, "All lambdas should be promoted."
    else:
        assert False
    return compl


def calcVarCompl(var: Var):
    # Complexity of var `f` is an lambda `λp1. ... λpn. T(p1,...,pn)`
    paramCount = var.varParamCount
    if paramCount > 0:
        lamParams = [makeLambdaParamSymbol() for _ in range(paramCount)]
        funcCompl = makeComplSymbol(var.varName, lamParams)
        return currying(lamParams, funcCompl)
    else:
        return 0


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    # appArg = makeVarSymbol(app.appArg.varName)
    appArg = symbols("var")
    return appArgCompl + appExprCompl.rcall(appArg)


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    caseAltsCompl = [calcExprCompl(alt, curFunc) for alt in case_.caseAlts]
    maxAltCompl = maxN(caseAltsCompl)
    return caseExprCompl + maxAltCompl


def currying(lamParams: List[Symbol], lamExpr):
    if len(lamParams) == 0:
        return lamExpr
    elif len(lamParams) == 1:
        return Lambda(lamParams[0], lamExpr)
    else:
        result = Lambda(lamParams[-1], lamExpr)
        for p in reversed(lamParams[:-1]):
            result = Lambda(p, result)
        return result


def maxN(args: List[Any]):
    assert len(args) > 0
    if len(args) == 1:
        return args[0]
    elif len(args) == 2:
        return MaxCompl(args[0], args[1])
    else:
        result = MaxCompl(args[-2], args[-1])
        for arg in reversed(args[:-2]):
            result = MaxCompl(arg, result)
        return result
