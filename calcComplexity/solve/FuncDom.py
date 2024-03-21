from typing import Dict, Set

# Complexity functions are represented by their names
# A dom B, means evaluating A is prior to evaluating B
nextOf: Dict[str, Set[str]] = {}
nodeUUID: Dict[str, int] = {}
uuid = 0


def addDomEdge(u: str, v: str):
    global uuid

    if u not in nextOf:
        nextOf[u] = {v}
        nodeUUID[u] = uuid
        uuid += 1
    else:
        nextOf[u].add(v)

    if v not in nextOf:
        nextOf[v] = set()
        nodeUUID[v] = uuid
        uuid += 1


def getDoms(u: str) -> Set[str]:
    return nextOf[u]


def outputGraph():
    from graphviz import Digraph

    # T_RealName'uuid
    def getRealName(s: str) -> str:
        end = len(s) - 1
        while s[end] != "'":
            end -= 1
        return s[2:end]

    graph = Digraph()

    for u in nextOf.keys():
        graph.node(str(nodeUUID[u]), label=getRealName(u))

    for u, vs in nextOf.items():
        for v in vs:
            graph.edge(str(nodeUUID[u]), str(nodeUUID[v]))

    graph.render("stat/func_dom_graph", format="png", cleanup=True)
