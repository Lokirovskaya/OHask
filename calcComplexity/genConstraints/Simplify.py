from typing import Dict, List

from calcComplexity.Log import logln
from calcComplexity.constraint import Constraint, ExprSymbol
import calcComplexity.constraint.Symbols as symbols
import calcComplexity.untypedLambdaCalculus as lam
import calcComplexity.haskellStruct as haskell


def simplify(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    # Remember to update exprSymbolList in every simplification funcs
    replaceConstSymbols(constrList, exprSymbolList)
    mergeIdenticalExprs(constrList, exprSymbolList)

    logln("[Simplified Constraints]")
    for constr in constrList:
        logln(str(constr))
    logln()
    logln("[Simplified Exprs]")
    for exprSym in exprSymbolList:
        logln(exprSym.name + " = " + str(exprSym.exprInfo.expr))
    logln()


def replaceConstSymbols(constrList: List[Constraint], exprSymbolList: List[ExprSymbol]):
    constExprSyms: List[ExprSymbol] = []
    newExprSymbolList = []
    for sym in exprSymbolList:
        if len(sym.exprInfo.dependsOnCrit) == 0:
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
