from __future__ import annotations
from dataclasses import dataclass
from typing import Any

from calcComplexity.constraint import SympyConstraint


@dataclass
class Graph:
    constr: SympyConstraint
    root: Node


@dataclass
class Node:
    cost: Any  # sympy expr

    def __str__(self) -> str:
        return str(self.cost)
