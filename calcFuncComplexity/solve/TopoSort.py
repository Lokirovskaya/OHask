from sympy import Symbol
from typing import Any, List, Optional, Set, Tuple, Dict
from queue import SimpleQueue
from ..preprocess.GenConstraints import Constraint
from .DependencyGraph import SymbolNode


def topoSortDepGraph(symNodeDict: Dict[Symbol, SymbolNode]) -> List[Constraint]:
    result = []
    nodesLeft = set()  # Ruled out nodes because it consists a ring

    queue = SimpleQueue()

    for node in symNodeDict.values():
        if node.childLeft == 0:
            queue.put(node)

    while not queue.empty():
        node: SymbolNode = queue.get()
        result.append(node.getConstraint())
        
        for parent in node.parentSet:
            parent.childLeft -= 1
            if parent.childLeft == 0:
                queue.put(parent)
                
    return result
