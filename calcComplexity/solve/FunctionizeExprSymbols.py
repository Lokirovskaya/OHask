from typing import Dict, List

import sympy

from calcComplexity.Log import logln
from calcComplexity.constraint import SympyConstraint

from .ScaleRelation import ScaleRelation


def functionizeExprSymbols(
    constrList: List[SympyConstraint],
    exprSymScaleRelationDict: Dict[str, ScaleRelation],
):
    replaceDict = {}
    for name, rel in exprSymScaleRelationDict.items():
        sym = sympy.Symbol(name)
        funcSym = sympy.Function(sym)(*rel.getParamSymbols())
        replaceDict[sym] = funcSym

    for constr in constrList:
        constr.rhs = constr.rhs.subs(replaceDict)

    logln("[Functionized Expr Symbols Constraints]")
    for constr in constrList:
        logln(str(constr))
    logln()
