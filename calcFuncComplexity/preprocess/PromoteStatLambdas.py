from typing import List, Tuple

from .Api import Func, Expr, Var, Lam


# Replace a lambda-expr with a var, promote lambda to a root-level function
# Promotion runs recursively
def promoteStatLambdas(funcList: List[Func]):
    for func in funcList:
        runExpr(func.funcExpr, funcList)


def runExpr(expr: Expr, funcList: List[Func]):
    if app := expr.matchApp():
        if lam := app.appExpr.matchLam():
            lamVar, lamFunc = promote(lam)
            app.appExpr = lamVar
            funcList.append(lamFunc)
            runExpr(lamFunc.funcExpr, funcList)
        if lam := app.appArg.matchLam():
            lamVar, lamFunc = promote(lam)
            app.appArg = lamVar
            funcList.append(lamFunc)
            runExpr(lamFunc.funcExpr, funcList)

    elif case_ := expr.matchCase():
        if lam := case_.caseExpr.matchLam():
            lamVar, lamFunc = promote(lam)
            case_.caseExpr = lamVar
            funcList.append(lamFunc)
            runExpr(lamFunc.funcExpr, funcList)
        for i in range(len(case_.caseAlts)):
            if lam := case_.caseAlts[i].matchLam():
                lamVar, lamFunc = promote(lam)
                case_.caseAlts[i] = lamVar
                funcList.append(lamFunc)
                runExpr(lamFunc.funcExpr, funcList)

    elif expr.matchLam():
        assert False, "Top-most level of expr should not be Lam"

    for child in expr.children():
        runExpr(child, funcList)


idx = 0


# return: (lambda var, promoted lambda)
def promote(lam: Lam) -> Tuple[Var, Func]:
    global idx
    uniqueName = ".lam" + str(idx)
    idx += 1
    paramTypes = [param.paramType for param in lam.lamParams]
    var = Var(
        data={},
        varName=uniqueName,
        varDisplayName=uniqueName,
        varType="",
        varParamTypes=paramTypes,
    )
    func = Func(
        data={},
        funcName=uniqueName,
        funcDisplayName=uniqueName,
        funcType="",
        funcParams=lam.lamParams,
        funcExpr=lam.lamExpr,
    )
    return (var, func)
