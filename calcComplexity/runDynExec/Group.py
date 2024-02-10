from typing import Dict, List, Tuple

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint.Symbols import ExprSymbol
import calcComplexity.haskellStruct as haskell


class Group:
    def __init__(
        self, domVars: Tuple[haskell.Var, ...], exprSymList: List[ExprSymbol]
    ) -> None:
        self.domVars = domVars
        self.exprSymList = exprSymList


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
    with open(LOG_PATH, "a") as f:
        f.write("[Dyn Exec Groups]\n")
        for i, group in enumerate(groupList):
            f.write(f"group{i} = {group.domVars} -> {group.exprSymList}\n")
        f.write("\n")

    return groupList
