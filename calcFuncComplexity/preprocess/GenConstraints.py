from sympy import Symbol
from typing import Any, Dict, List
from .LazyLambda import LazyAdd, LazyApply
from .SymbolMaker import makeComplSymbol, makeParamSymbol, makeScaleSymbol
from .Api import Func, Expr, Var, App, Case


class Constraint:
    def __init__(self, lhs: Symbol, rhs: Any):
        self.lhs = lhs
        self.rhs = rhs  # None means external symbol

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Constraint):
            return False
        return self.lhs == __value.lhs

    def __hash__(self) -> int:
        return hash(self.lhs)


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
        complSymbol = makeComplSymbol(func.funcName)
        complValue = calcFuncCompl(func)
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
    return varsymDict[var.varName]


def calcAppCompl(app: App, curFunc: Func):
    appExprCompl = calcExprCompl(app.appExpr, curFunc)
    appArgCompl = calcExprCompl(app.appArg, curFunc)
    return LazyApply(appExprCompl, appArgCompl)


def calcCaseCompl(case_: Case, curFunc: Func):
    caseExprCompl = calcExprCompl(case_.caseExpr, curFunc)
    caseAltCompl = calcExprCompl(case_.caseAlts[-1], curFunc)
    return LazyAdd(caseExprCompl, caseAltCompl)
