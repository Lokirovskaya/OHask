from typing import List

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import Constraint, ExprSymbol
import calcComplexity.constraint.Symbols as symbols
from calcComplexity.haskellStruct.Struct import Var
import calcComplexity.untypedLambdaCalculus as lam


def simplify(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    # Remember to update exprSymbolList in every simplification funcs
    replaceConstSymbols(constrList, exprSymbolList)

    with open(LOG_PATH, "a") as f:
        f.write("[Simplified Constraints]\n")
        for constr in constrList:
            f.write(str(constr) + "\n")
        f.write("\n")
        f.write("[Simplified Exprs]\n")
        for exprSym in exprSymbolList:
            f.write(exprSym.name + ": " + str(exprSym.exprInfo) + "\n")
        f.write("\n")


def replaceConstSymbols(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    constExprSyms: List[ExprSymbol] = []
    newExprSymbolList = []
    for sym in exprSymbolList:
        if len(sym.exprInfo.dependsOn) == 0:
            constExprSyms.append(sym)
        else:
            newExprSymbolList.append(sym)

    replDict = {sym: symbols.constant() for sym in constExprSyms}
    for constr in constrList:
        # replDict should be covariant here bcz I won't modify it
        lam.replaceVarDict(constr.rhs, replDict)  # type: ignore

    exprSymbolList[:] = newExprSymbolList
