from __future__ import annotations
from sympy import Symbol, Function, preorder_traversal
from typing import Any, List, Optional, Set, Dict

from ..struct.DepGraphNode import DepGraphNode
from ..struct.Constraint import Constraint


def buildDepGraph(constraintList: List[Constraint]) -> Dict[str, DepGraphNode]:
    depNodeDict: Dict[str, DepGraphNode] = {}

    # Add defined complexity symbol
    for cons in constraintList:
        assert cons.lhs.name not in depNodeDict, f"Duplicated lhs {cons.lhs.name}."
        symNode = DepGraphNode(cons)
        depNodeDict[cons.lhs.name] = symNode

    # Add external complexity symbol
    # Fill the field depSymNodeSet of nodes
    for cons in constraintList:
        lhs = cons.lhs
        rhs = cons.rhs
        assert rhs != None
        varOccur = list(rhs.free_symbols) + [
            node for node in preorder_traversal(rhs) if isinstance(node, Function)
        ]
        for depSym in varOccur:
            # if not isinstance(depSym, Symbol):
            # continue
            # An external compl sym
            if depSym.name not in depNodeDict:
                depNodeDict[depSym.name] = DepGraphNode(Constraint(depSym, None))

            depNodeDict[lhs.name].addDepSymNode(depNodeDict[depSym.name])

    return depNodeDict
