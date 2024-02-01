from typing import Dict, List, Set

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import ExprInfo, ExprSymbol
import calcComplexity.haskellStruct as haskell

# Params of all funcs
criticalVarSet: Set[haskell.Var] = set()


# fill the field ExprInfo.dependsOn
def findVarDep(funcList: List[haskell.Func], exprSymbolList: List[ExprSymbol]):
    for func in funcList:
        criticalVarSet.update(func.funcParams)
        buildDepGraph(func)

    with open(LOG_PATH, "a") as f:
        f.write("[Var Dependencies]\n")
        for var, dep in var2VarDepDict.items():
            f.write(f"{var} -> {dep.dependsOn}\n")
        f.write("\n")

    # fill the field ExprInfo.dependsOn
    for exprSym in exprSymbolList:
        fillExprDepInfo(exprSym.exprInfo)

    with open(LOG_PATH, "a") as f:
        f.write("[Exprs]\n")
        for exprSym in exprSymbolList:
            f.write(exprSym.name + ": " + str(exprSym.exprInfo) + "\n")
        f.write("\n")


# Node of dep graph
# Var dep relations are only determined by:
#   1. Pattern match exprs,
#        case {v1, v2} of
#          Con v3 v4 -> _
#      Expr above tells relations that v3->{v1, v2} and v4->{v1, v2}.
#   2. Zero-param function,
#        f = x v
#      Expr above tells relations that f->{x, v}.
# Note: Var itself is not included in set `dependsOn` (but it should be)
class VarDep:
    def __init__(self, var: haskell.Var) -> None:
        self.var = var
        self.dependsOn: Set[haskell.Var] = set()


var2VarDepDict: Dict[haskell.Var, VarDep] = {}


def addRelations(src: haskell.Var, dstSet: Set[haskell.Var]):
    if src not in var2VarDepDict:
        var2VarDepDict[src] = VarDep(src)
    var2VarDepDict[src].dependsOn.update(dstSet)


def buildDepGraph(func: haskell.Func):
    # Zero-param function
    if func.funcParamCount == 0:
        addRelations(func.varLike, haskell.getAllVars(func.funcExpr))

    # Pattern match exprs
    for case_ in haskell.preOrderTraversal(func.funcExpr):
        if not isinstance(case_, haskell.Case):
            continue

        caseExprVars = haskell.getAllVars(case_.caseExpr)
        for alt in case_.caseAlts:
            for conVar in alt.altConVars:
                # conVar->{caseExprVars}
                addRelations(conVar, caseExprVars)


# Find all vars which dominate the var, recursively
def findAllDeps(var: haskell.Var) -> Set[haskell.Var]:
    if var not in var2VarDepDict:
        return {var}
    else:
        ans = {var}.union(var2VarDepDict[var].dependsOn)
        for dep in var2VarDepDict[var].dependsOn:
            ans.update(findAllDeps(dep))
        return ans


def fillExprDepInfo(exprInfo: ExprInfo):
    for var in exprInfo.varSet:
        allDeps = findAllDeps(var)
        depsAlsoCritical = allDeps.intersection(criticalVarSet)
        exprInfo.dependsOn.update(depsAlsoCritical)
