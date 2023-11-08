import sympy
from typing import List, Dict, Any

from .LazyLambda import LazyApply
from .SymbolizeVars import symbolize
from .Api import Func, Param, Expr, Var, Lit, App, Case
from .Util import zEncode

varsymDict: Dict[str, Any]


def calcCompl(funcList: List[Func]):
    global varsymDict
    varsymDict = symbolize(funcList)
    print(varsymDict)

    for func in funcList:
        compl = calcFuncCompl(func)
        print(f"O({func.funcName}) = {compl}")


def calcFuncCompl(func: Func):
    compl = calcExprCompl(func.funcExpr, func)
    return compl


def calcExprCompl(expr: Expr, curFunc: Func):
    if var := expr.matchVar():
        compl = calcVarCompl(var, curFunc)
    elif lit := expr.matchLit():
        compl = calcLitCompl(lit, curFunc)
    elif app := expr.matchApp():
        compl = calcAppCompl(app, curFunc)
    elif case_ := expr.matchCase():
        compl = calcCaseCompl(case_, curFunc)
    else:
        assert False
    return compl


def calcVarCompl(var: Var, curFunc: Func):
    return varsymDict[var.varName]


def calcLitCompl(lit: Lit, curFunc: Func):
    return 1


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    return LazyApply(appExprCompl, appArgCompl)


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    caseAltCompl = calcExprCompl(case_.caseAlts[-1], curFunc)
    return caseExprCompl + caseAltCompl
