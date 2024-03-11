from typing import Dict, List, Set, Tuple

from calcComplexity.Log import logln
from calcComplexity.constraint.Symbols import ExprSymbol
import calcComplexity.haskellStruct as haskell
import calcComplexity.genConstraints.VarDep as varDep


class Group:
    _groupIdx = 0

    def __init__(
        self, paramVars: Tuple[haskell.Var, ...], exprSymList: List[ExprSymbol]
    ) -> None:
        self.groupIdx = Group._groupIdx
        Group._groupIdx += 1
        self.groupName = "g" + str(self.groupIdx)
        self.paramVars = paramVars
        self.exprSymList = exprSymList
        # Dominant by param vars
        self.domVars: Set[haskell.Var] = set()
        for param in paramVars:
            self.domVars.update(varDep.getAllDoms(param))


def makeGroups(exprSymList: List[ExprSymbol]) -> List[Group]:
    vars2GroupDict: Dict[Tuple[haskell.Var, ...], Group] = {}
    for sym in exprSymList:
        tupDep = tuple(sym.exprInfo.dependsOnCrit)
        if tupDep in vars2GroupDict:
            group = vars2GroupDict[tupDep]
            group.exprSymList.append(sym)
        else:
            group = Group(tupDep, [sym])
            vars2GroupDict[tupDep] = group

    groupList = list(vars2GroupDict.values())

    logln("[Dyn Exec Groups]")
    for group in groupList:
        logln(
            f"group{group.groupIdx}: paramVars={group.paramVars}, exprs={group.exprSymList}, domVars={group.domVars}"
        )
    logln()

    return groupList
