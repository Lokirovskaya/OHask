from __future__ import annotations
from sympy import Symbol, Function, preorder_traversal
from typing import List, Dict

from ...struct import DepGraphNode, Constraint


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
        funcOccurs = [
            node
            for node in preorder_traversal(rhs)
            if isinstance(node, Function) and node.name.startswith("T_")
        ]
        # lhs depends on every funcs
        for func in funcOccurs:
            depNodeDict[lhs.name].addDepSymNode(depNodeDict[func.name])

    return depNodeDict
