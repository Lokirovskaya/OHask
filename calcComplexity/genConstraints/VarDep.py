from typing import Dict, List, Set

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import ExprInfo
import calcComplexity.haskellStruct as haskell
import calcComplexity.untypedLambdaCalculus as lam

# Params of all funcs
criticalVarSet: Set[haskell.Var] = set()


# fill the field ExprInfo.dependsOn
def findVarDep(funcList: List[haskell.Func], exprSymbolList: List[lam.Var]):
    for func in funcList:
        criticalVarSet.update(func.funcParams)
        buildDepGraph(func.funcExpr)

    with open(LOG_PATH, "a") as f:
        f.write("[Var Dependencies]\n")
        for var, dep in var2VarDepDict.items():
            f.write(f"{var} -> {dep.dependsOn}\n")
        f.write("\n")

        f.write("[Exprs]\n")
        for exprSym in exprSymbolList:
            exprInfo = exprSym.kwargs["exprInfo"]
            f.write(exprSym.name + ": " + str(exprInfo) + "\n")
        f.write("\n")


# Node of dep graph
# Var dep relations are only determined by pattern match exprs.
#   case {v1, v2} of
#     Con v3 v4 -> _
# Expr above tells relations that v3->{v1, v2} and v4->{v1, v2}.
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


def buildDepGraph(expr: haskell.Expr):
    for case_ in haskell.preOrderTraversal(expr):
        if not isinstance(case_, haskell.Case):
            continue

        caseExprVars = haskell.getAllVars(case_.caseExpr)
        for alt in case_.caseAlts:
            for conVar in alt.altConVars:
                # conVar->{caseExprVars}
                addRelations(conVar, caseExprVars)
