import re
from typing import Dict, List, Set, Tuple

import sympy

from calcComplexity.Log import logln
from calcComplexity.basicFuncs import basicFuncDict
from calcComplexity.constraint import SympyConstraint
import calcComplexity.haskellStruct as haskell
from calcComplexity.regression import LinearResult
from calcComplexity.runDynExec import Group
from calcComplexity.solve.util.SympySymbols import (
    makeScaleSymbol,
    makeSymbol,
    makeUnknownSymbol,
)
import calcComplexity.untypedLambdaCalculus as lam

from .ScaleRelation import ScaleRelation

r2Thresh = 0.99


# See [Linear Result], [Dyn Exec Groups], [Param Lookup Table] in calc_log.txt
def lookupExprSymScaleRelations(
    sympyConstrList: List[SympyConstraint],
    regressionResultList: List[LinearResult],
    groupList: List[Group],
    paramH2LTable: Dict[haskell.Var, lam.Var],
) -> Dict[str, ScaleRelation]:

    namesOfExprSymsOccur: Set[str] = set()

    def lookupOneRelation(
        regResult: LinearResult,
    ) -> Tuple[str, SympyConstraint] | None:
        # S_ListLen(e0) = c + coef*i0 + ...

        # get lhs
        groupIdx = regResult.groupIdx
        group = groupList[groupIdx]
        outputVarIdx, outputScaleName = parseRegVarName(regResult.y, isInput=False)
        exprVarName = group.exprSymList[outputVarIdx].name
        if exprVarName not in namesOfExprSymsOccur:
            # Current expr sym not occurs in any constraints (may be reduced by `reduceBuiltinFunctions`)
            # Need not further calculation
            return None
        exprSymbol = makeSymbol(exprVarName)
        scaleSymbol = makeScaleSymbol(outputScaleName)
        lhs = scaleSymbol(exprSymbol)

        # get rhs
        if abs(regResult.r2) < r2Thresh:
            rhs = makeUnknownSymbol()
        else:
            rhs = round(regResult.xConst, 3)
            for xName, coef in regResult.xTerms:
                # One term of rhs
                # coef * basicFuncLambda(*InputSymbolWithScaleList)
                inputVarInfoList, basicFuncName = parseInputVarName(xName)
                basicFuncLambda = basicFuncDict[basicFuncName].sympyLambda
                inputSymbolWithScaleList = []
                for inputVarIdx, inputScaleName in inputVarInfoList:
                    inputHVar = group.paramVars[inputVarIdx]
                    inputLVar = paramH2LTable[inputHVar]
                    inputSymbol = makeSymbol(inputLVar.name)  # todo: param arity
                    scaleSymbol = makeScaleSymbol(inputScaleName)
                    inputSymbolWithScale = scaleSymbol(inputSymbol)
                    inputSymbolWithScaleList.append(inputSymbolWithScale)
                coef = round(coef, 3)
                rhsTerm = coef * basicFuncLambda(*inputSymbolWithScaleList)
                rhs += rhsTerm

        constr = SympyConstraint(lhs, rhs)

        return exprVarName, constr

    # Fill `namesOfExprSymsOccur`
    for constr in sympyConstrList:
        exprSyms = filter(
            lambda s: s.startswith("e"),
            [sym.name for sym in constr.rhs.atoms(sympy.Symbol)],
        )
        namesOfExprSymsOccur.update(exprSyms)

    result: Dict[str, ScaleRelation] = {}

    def appendResult(name, relation):
        if name not in result:
            result[name] = ScaleRelation(name, [])
        result[name].constrList.append(relation)

    for regResult in regressionResultList:
        r = lookupOneRelation(regResult)
        if r != None:
            exprSymName, relationConstr = r
            appendResult(exprSymName, relationConstr)

    logln("[Expr Symbol Scale Relations]")
    for rel in result.values():
        logln(str(rel))
    logln()

    return result


# Names like: o0IntVal, o1ListLen, i0ListLen
# return: idx, scaleName
def parseRegVarName(name: str, isInput: bool) -> Tuple[int, str]:
    if isInput:
        m = re.match(r"i(\d+)(\w+)", name)
    else:
        m = re.match(r"o(\d+)(\w+)", name)
    assert m != None, f"Nothing matches in '{name}'"
    idx = int(m.group(1))
    scaleName = m.group(2)
    return idx, scaleName


# Names like: i0ListLen$i1ListLen$xy
# return: [(idx, scaleName)], basicFuncName
def parseInputVarName(name: str) -> Tuple[List[Tuple[int, str]], str]:
    splits = name.split("$")
    basicFuncName = splits[-1]
    l = []
    for s in splits[:-1]:
        idx, scaleName = parseRegVarName(s, isInput=True)
        l.append((idx, scaleName))
    return l, basicFuncName
