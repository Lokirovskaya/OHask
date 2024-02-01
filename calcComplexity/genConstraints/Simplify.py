from typing import Dict, List

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import Constraint, ExprSymbol
import calcComplexity.constraint.Symbols as symbols
import calcComplexity.untypedLambdaCalculus as lam
import calcComplexity.haskellStruct as haskell


def simplify(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    # Remember to update exprSymbolList in every simplification funcs
    replaceConstSymbols(constrList, exprSymbolList)
    mergeIdenticalExprs(constrList, exprSymbolList)

    with open(LOG_PATH, "a") as f:
        f.write("[Simplified Constraints]\n")
        for constr in constrList:
            f.write(str(constr) + "\n")
        f.write("\n")
        f.write("[Simplified Exprs]\n")
        for exprSym in exprSymbolList:
            f.write(exprSym.name + " = " + str(exprSym.exprInfo.expr) + "\n")
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


def mergeIdenticalExprs(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    replDict: Dict[ExprSymbol, ExprSymbol] = {}

    expr2SymDict: Dict[haskell.Expr, ExprSymbol] = {}
    for sym in exprSymbolList:
        if sym.exprInfo.expr not in expr2SymDict:
            expr2SymDict[sym.exprInfo.expr] = sym
        else:
            # replace curSym to prevSym
            replDict[sym] = expr2SymDict[sym.exprInfo.expr]

    for constr in constrList:
        lam.replaceVarDict(constr.rhs, replDict)  # type: ignore

    exprSymbolList[:] = filter(lambda sym: sym not in replDict, exprSymbolList)
