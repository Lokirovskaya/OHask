from sympy import Symbol
from typing import List, Tuple, Dict
from queue import SimpleQueue
from ..preprocess.GenConstraints import Constraint
from .DependencyGraph import SymbolNode


def topoSortDepGraph(
    symNodeDict: Dict[Symbol, SymbolNode]
) -> Tuple[List[Constraint], List[Constraint]]:
    
    result = []
    # Recursive constraints, each should be one of:
    # 1. Node of a loop in dependency graph;
    # 2. Depends on a recursive function
    recConstrSet = set([node.constraint for node in symNodeDict.values()])

    queue = SimpleQueue()

    for node in symNodeDict.values():
        if node.childLeft == 0:
            queue.put(node)

    while not queue.empty():
        node: SymbolNode = queue.get()
        result.append(node.constraint)

        for parent in node.parentSet:
            parent.childLeft -= 1
            if parent.childLeft == 0:
                queue.put(parent)

    for node in result:
        recConstrSet.remove(node)

    return (result, list(recConstrSet))
