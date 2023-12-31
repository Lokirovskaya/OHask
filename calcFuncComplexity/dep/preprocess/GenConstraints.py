from typing import Any, List, Set
from calcFuncComplexity.struct import Constraint, MaxCompl, MyLambda
from calcFuncComplexity.util.symbol import (
    makeCompl,
    makeLambdaParam,
    makeParam,
    makeExternalParam,
    makeVar,
    makeExternalPlaceholder,
    makeLit,
)
from .Api import Func, Expr, Var, App, Case
from calcFuncComplexity.util.log import log

constraintList: List[Constraint]
curConstrNames: Set[str]

# Replace param-var-symbols(v_x or f_x) with param-symbols(pi_x)
# varParamMap: Dict[str, ]


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
        funcCompl = calcFuncCompl(func)

        # Replace param-var-symbols(v_x or f_x) with param-symbols(pi_x)
        paramSymbols = [
            makeVar(param.paramName, isFunc=(param.paramArity > 0))
            for param in func.funcParams
        ]
        indexedParamSymbol = [
            makeParam(func.funcName, i, isFunc=(param.paramArity > 0))
            for i, param in enumerate(func.funcParams)
        ]
        assert len(indexedParamSymbol) == len(paramSymbols)
        for i in range(func.funcParamCount):
            funcCompl = funcCompl.replace(paramSymbols[i], indexedParamSymbol[i])

        # make constraint
        # Constraint of a function looks like:
        #   T_f = λp0. λp1. expr
        complSymbol = makeCompl(func.funcName)  # lhs
        complValue = makeLambda(indexedParamSymbol, funcCompl)  # rhs
        constraintList.append(Constraint(complSymbol, complValue))

    log("[Raw Constraints]")
    for con in constraintList:
        log(con)
    log()

    return constraintList


def makeLambda(params, expr):
    return MyLambda(tuple(params), expr)


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
    paramCount = var.varArity
    if paramCount > 0:
        name = var.varName
        lamParams = [makeLambdaParam() for _ in range(paramCount)]
        funcCompl = makeCompl(name)

        # External function call
        if name not in curConstrNames:
            curConstrNames.add(name)
            paramSymbols = [makeExternalParam(name, i) for i in range(paramCount)]
            constraintList.append(
                Constraint(
                    funcCompl,
                    makeLambda(paramSymbols, makeExternalPlaceholder()),
                )
            )

        return makeLambda(lamParams, funcCompl(*lamParams))
    else:
        return 1


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    appArg = getSymbolExpr(app.appArg)
    return 1 + appArgCompl + appExprCompl.rcall(appArg)


def getSymbolExpr(expr: Expr):
    if var := expr.matchVar():
        if var.varArity > 0:
            func = makeVar(var.varName, isFunc=True)
            lamParams = [makeLambdaParam() for _ in range(var.varArity)]
            return makeLambda(lamParams, func(*lamParams))
        else:
            return makeVar(var.varName)
    elif lit := expr.matchLit():
        return makeLit(lit.litValue, lit.litType)
    elif app := expr.matchApp():
        func = getSymbolExpr(app.appExpr)
        arg = getSymbolExpr(app.appArg)
        return func(arg)
    elif case_ := expr.matchCase():
        return 0  # todo
    elif expr.matchLam():
        assert False, "All lambdas should be promoted."
    else:
        assert False


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    # Most of the case-exprs have at least 1 case-alt, 
    # but some GHC-inserted exception handling exprs may not.
    if case_.caseAltCount > 0:
        caseAltsCompl = [calcExprCompl(alt, curFunc) for alt in case_.caseAlts]
        maxAltCompl = maxN(caseAltsCompl)
        return caseExprCompl + maxAltCompl
    else:
        return caseExprCompl


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
