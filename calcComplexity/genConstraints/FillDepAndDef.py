from typing import Dict, List, Set

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import ExprInfo, ExprSymbol
import calcComplexity.haskellStruct as haskell
from . import VarDep as varDep
from . import VarDef as varDef

# Params of all funcs
criticalVarSet: Set[haskell.Var] = set()


def fillDepAndDef(funcList: List[haskell.Func], exprSymbolList: List[ExprSymbol]):
    # 1. Build dep tree, see VarDep.py
    # 2. Fill def info, see VarDef.py
    for func in funcList:
        criticalVarSet.update(func.funcParams)
        runFunc(func)

    # Fill the field `ExprInfo.dependsOnCrit`
    for exprSym in exprSymbolList:
        fillExprDepInfo(exprSym.exprInfo)

    with open(LOG_PATH, "a") as f:
        f.write("[Exprs]\n")
        for exprSym in exprSymbolList:
            f.write(f"{exprSym.name}: {exprSym.exprInfo}\n")
        f.write("\n")

        f.write("[Var Defs]\n")
        for var, s in varDef.defOfVar.items():
            f.write(f"{var} = {s}\n")
        f.write("\n")


# Theory detail: See VarDep.py & VarDef.py
def runFunc(func: haskell.Func):
    # Zero-param function
    if func.funcParamCount == 0:
        varDep.addDepEdges(func.varLike, haskell.getAllVars(func.funcExpr))
        varDef.addDefExpr(func.varLike, func.funcExpr)

    # Pattern match exprs
    for case_ in haskell.preOrderTraversal(func.funcExpr):
        if not isinstance(case_, haskell.Case):
            continue

        # Dep: conVar -> {caseExprVars}
        caseExprVars = haskell.getAllVars(case_.caseExpr)
        for alt in case_.caseAlts:
            for conVar in alt.altConVars:
                varDep.addDepEdges(conVar, caseExprVars)

        varDef.addDefCase(case_)


def fillExprDepInfo(exprInfo: ExprInfo):
    for var in exprInfo.varSet:
        allDeps = varDep.getAllDeps(var)
        depsAlsoCritical = allDeps.intersection(criticalVarSet)
        exprInfo.dependsOnCrit.update(depsAlsoCritical)
