from typing import List, Dict, Set, Any
from sympy import Lambda, Symbol
from .Api import Func, Expr, Var
from .SymbolMaker import (
    makeComplSymbol,
    makeScaleSymbol,
    makeLambdaParamSymbol,
)

# Symbolize complexity of vars and functions
# Complexity is defined as computational step.
# Complexity of function `f(a1,...,an)` is an n-ary function `O(a1,...,an)`
# Complexity of var `f` is an lambda `λp1. ... λpn. O(a1,...,an)[a1 -> p1, ..., an -> pn]`


def symbolize(funcList: List[Func]):
    varSet = findAllVars(funcList)
    varsymDict = defSymbolForVars(varSet)
    return varsymDict


def findAllVars(funcList: List[Func]) -> Set[Var]:
    varSet = set()

    def runExpr(expr: Expr):
        if var := expr.matchVar():
            varSet.add(var)
        else:
            for c in expr.children():
                runExpr(c)

    for func in funcList:
        runExpr(func.funcExpr)

    return varSet


def defSymbolForVars(varSet: Set[Var]) -> Dict[str, Any]:
    varsymDict = {}
    for var in varSet:
        varsym = defVarSymbol(var)
        assert var.varName not in varsymDict, f"Duplicated var {var.varName}"
        varsymDict[var.varName] = varsym
    return varsymDict


def defVarSymbol(var: Var) -> Any:
    name = var.varName

    # Complexity of var `f` is an lambda `λp1. ... λpn. O(a1,...,an)[a1 -> p1, ..., an -> pn]`
    paramCount = var.varParamCount
    if paramCount > 0:
        lamParams = [makeLambdaParamSymbol() for _ in range(paramCount)]
        funcCompl = makeComplSymbol(name, lamParams)
        return currying(lamParams, funcCompl)
    else:
        return makeScaleSymbol(name)


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
