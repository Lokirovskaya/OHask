from typing import Dict, List

import igraph
import sympy

from calcComplexity.constraint import SympyConstraint

graph = igraph.Graph(directed=True)
name2Constr: Dict[str, SympyConstraint] = {}


def markRecCalls(constrList: List[SympyConstraint]):
    for constr in constrList:
        name2Constr[constr.lhs.name] = constr

    buildGraph(constrList)
    allSCC = findSCCs()


def buildGraph(constrList: List[SympyConstraint]):
    for lhs, _ in constrList:
        name = lhs.name
        graph.add_vertex(name)

    for lhs, rhs in constrList:
        uName = lhs.name
        vNames = filter(
            lambda name: name.startswith("T_"),
            [func.name for func in rhs.atoms(sympy.Function)],
        )
        for vName in vNames:
            # Disallowing external nodes (funcs) and duplicate edges (multi calls)
            if vName not in graph.vs["name"]:
                continue
            if graph.are_connected(uName, vName):
                continue
            graph.add_edge(uName, vName)


# Find strongly connected components
# exclude single nodes without a self-loop.
def findSCCs() -> List[List[str]]:
    results: List[List[str]] = []

    allSCC = graph.connected_components(mode="strong")

    def getName(i):
        return graph.vs[i]["name"]

    for scc in allSCC:
        if len(scc) == 1:
            if graph.are_connected(scc[0], scc[0]):
                name = getName(scc[0])
                results.append([name])
        else:
            names = list(map(getName, scc))
            results.append(names)

    return results
