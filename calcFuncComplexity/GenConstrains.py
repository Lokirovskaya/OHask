import sympy
from typing import Any, Dict, List, Tuple
from .LazyLambda import LazyAdd, LazyApply
from .SymbolMaker import makeComplSymbol
from .Api import Func, Expr, Var, App, Case

varsymDict: Dict[str, Any]


# List of equations
# [(lhs, rhs), ...]
def genConstrainList(
    funcList: List[Func], _varsymDict: Dict[str, Any]
) -> List[Tuple[sympy.Symbol, Any]]:
    global varsymDict
    varsymDict = _varsymDict
    
    constrainList = []
    for func in funcList:
        complSymbol = makeComplSymbol(func.funcName)
        complValue = calcFuncCompl(func)
        constrainList.append((complSymbol, complValue))
    return constrainList


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
    else:
        assert False
    return compl


def calcVarCompl(var: Var):
    return varsymDict[var.varName]


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    return LazyApply(appExprCompl, appArgCompl)


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    caseAltCompl = calcExprCompl(case_.caseAlts[-1], curFunc)
    return LazyAdd(caseExprCompl, caseAltCompl)
