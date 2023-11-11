from sympy import Symbol
from typing import List, Tuple, Dict
from queue import SimpleQueue
from ..struct.Constraint import Constraint
from ..struct.DepGraphNode import DepGraphNode


def topoSortDepGraph(
    depNodeDict: Dict[str, DepGraphNode]
) -> Tuple[List[Constraint], List[Constraint]]:
    result: List[Constraint] = []
    # Recursive constraints, each should be one of:
    # 1. Node of a loop in dependency graph;
    # 2. Depends on a recursive function
    recConstrSet = set([node.constraint for node in depNodeDict.values()])

    queue = SimpleQueue()

    for node in depNodeDict.values():
        if node.childLeft == 0:
            queue.put(node)

    while not queue.empty():
        node: DepGraphNode = queue.get()
        result.append(node.constraint)

        for parent in node.parentNodeSet:
            parent.childLeft -= 1
            if parent.childLeft == 0:
                queue.put(parent)

    for constr in result:
        recConstrSet.remove(constr)

    return (result, list(recConstrSet))
