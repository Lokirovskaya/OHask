# Var dep relations are only determined by:
#   1. Pattern match exprs,
#        case {v1, v2} of
#          Con v3 v4 -> _
#      Expr above tells relations that v3->{v1, v2} and v4->{v1, v2}.
#   2. Zero-param function,
#        f = x v
#      Expr above tells relations that f->{x, v}.

from typing import Dict, Iterable, Set
from calcComplexity.haskellStruct import Var

# An var dep tree
# Represented by adjacency set

nextOf: Dict[Var, Set[Var]] = {}


def addDepEdge(u: Var, v: Var):
    if u not in nextOf:
        nextOf[u] = {v}
    else:
        nextOf[u].add(v)


def addDepEdges(u: Var, vs: Iterable[Var]):
    for v in vs:
        addDepEdge(u, v)


def _getImmediateDeps(v: Var) -> Set[Var]:
    if v in nextOf:
        return nextOf[v]
    else:
        return set()


def getAllDeps(v: Var) -> Set[Var]:
    ans = set()

    def dfs(v: Var):
        ans.add(v)
        for vnext in _getImmediateDeps(v):
            if v != vnext:
                dfs(vnext)

    dfs(v)
    return ans
