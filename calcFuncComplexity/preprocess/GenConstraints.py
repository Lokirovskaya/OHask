from typing import Any, Dict, List, Set
from ..struct.Constraint import Constraint
from ..struct.MaxCompl import MaxCompl
from .SymbolMaker import (
    makeComplSymbol,
    makeLambdaParamSymbol,
    makeParamSymbol,
    makeVarSymbol,
    makePlaceholderSymbol,
)
from .Api import Func, Expr, Var, App, Case
from sympy import Function, Symbol, Lambda, symbols

constraintList: List[Constraint]
curConstrNames: Set[str]


# List of equations
# [(lhs, rhs), ...]
def genConstraintList(funcList: List[Func]) -> List[Constraint]:
    global constraintList
    constraintList = []
    global curConstrNames
    curConstrNames = set()

    # Init current constraint name set with all internal functions first
    for func in funcList:
        curConstrNames.add(func.funcName)

    for func in funcList:
        paramSymbols = [
            makeParamSymbol(func.funcName, i) for i in range(len(func.funcParams))
        ]
        complSymbol = makeComplSymbol(func.funcName)
        complValue = currying(paramSymbols, calcFuncCompl(func))
        constraintList.append(Constraint(complSymbol, complValue))

    return constraintList


def calcFuncCompl(func: Func):
    compl = calcExprCompl(func.funcExpr, func)
    return compl


def calcExprCompl(expr: Expr, curFunc: Func):
    if var := expr.matchVar():
        compl = calcVarCompl(var)
    elif expr.matchLit():
        compl = 1
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
        name = var.varName
        lamParams = [makeLambdaParamSymbol() for _ in range(paramCount)]
        funcCompl = makeComplSymbol(name)

        # External function call
        if name not in curConstrNames:
            curConstrNames.add(name)
            paramSymbols = [makeParamSymbol(name, i) for i in range(paramCount)]
            constraintList.append(
                Constraint(
                    funcCompl,
                    currying(paramSymbols, makePlaceholderSymbol()),
                )
            )

        return currying(lamParams, funcCompl(*lamParams))
    else:
        return 1


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    # appArg = makeVarSymbol(app.appArg.varName)
    appArg = symbols("var")
    return 1 + appArgCompl + appExprCompl.rcall(appArg)


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
    # max2 = MaxCompl("max")
    if len(args) == 1:
        return args[0]
    elif len(args) == 2:
        return MaxCompl(args[0], args[1])
    else:
        result = MaxCompl(args[-2], args[-1])
        for arg in reversed(args[:-2]):
            result = MaxCompl(arg, result)
        return result
