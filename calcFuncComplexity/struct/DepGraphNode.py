from __future__ import annotations
from typing import Set
from .Constraint import Constraint


class DepGraphNode:
    def __init__(self, constraint: Constraint):
        self.constraint = constraint
        self.depNodeSet: Set[DepGraphNode] = set()
        # Update when running topo-sort
        self.childLeft: int = 0
        self.parentNodeSet: Set[DepGraphNode] = set()

    def __str__(self) -> str:
        depSymNames = [node.constraint.lhs.name for node in self.depNodeSet]
        depSymStr = ", ".join(depSymNames)
        return f"{self.constraint.lhs.name} -> [{depSymStr}]"

    def addDepSymNode(self, node: DepGraphNode):
        self.depNodeSet.add(node)
        self.childLeft += 1
        node.parentNodeSet.add(self)
