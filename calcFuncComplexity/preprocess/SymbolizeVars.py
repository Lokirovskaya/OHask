from typing import List, Dict, Set, Any
import sympy
from .Api import Func, Expr, Var
from ..struct.LazyLambda import LazyLambda, LazySubstitute
from .SymbolMaker import (
    makeComplSymbol,
    makeParamSymbol,
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


def findAllVars(funcList: List[Func]) -> Dict[str, Var]:
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
        funcCompl = makeComplSymbol(name)

        funcParams = [makeParamSymbol(name, idx) for idx in range(paramCount)]
        lamParams = [makeLambdaParamSymbol() for _ in range(paramCount)]
        substList = [(funcParams[i], lamParams[i]) for i in range(paramCount)]

        lamExpr = LazySubstitute(funcCompl, substList)

        return currying(lamParams, lamExpr)

    else:
        return makeScaleSymbol(name)


def currying(lamParams: List[sympy.Symbol], lamExpr) -> LazyLambda:
    if len(lamParams) == 0:
        return lamExpr
    elif len(lamParams) == 1:
        return LazyLambda(lamParams[0], lamExpr)
    else:
        result = LazyLambda(lamParams[-1], lamExpr)
        for p in reversed(lamParams[:-1]):
            result = LazyLambda(p, result)
        return result
