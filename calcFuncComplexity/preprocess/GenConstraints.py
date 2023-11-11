from typing import Any, Dict, List
from ..struct.Constraint import Constraint
from .SymbolMaker import makeComplSymbol, makeParamSymbol, makeScaleSymbol
from .Api import Func, Expr, Var, App, Case


varsymDict: Dict[str, Any]


# List of equations
# [(lhs, rhs), ...]
def genConstraintList(
    funcList: List[Func], _varsymDict: Dict[str, Any]
) -> List[Constraint]:
    global varsymDict
    varsymDict = _varsymDict

    constraintList = []

    for func in funcList:
        # Var = Param constraints
        for idx in range(len(func.funcParams)):
            param = func.funcParams[idx]
            paramSymbol = makeParamSymbol(func.funcName, idx)
            varSymbol = makeScaleSymbol(param.paramName)
            constraintList.append(Constraint(varSymbol, paramSymbol))

        # FuncExpr constrains
        paramsSymbol = [
            makeParamSymbol(func.funcName, i)
            for i in range(len(func.funcParams))
        ]
        complSymbol = makeComplSymbol(func.funcName, paramsSymbol)
        complValue = calcFuncCompl(func)
        constraintList.append(Constraint(complSymbol, complValue)) # type: ignore

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
    return varsymDict[var.varName]


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    return appExprCompl(appArgCompl)


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    caseAltCompl = calcExprCompl(case_.caseAlts[-1], curFunc)
    return caseExprCompl + caseAltCompl
