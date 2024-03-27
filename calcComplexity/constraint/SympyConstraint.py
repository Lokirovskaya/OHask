from typing import Any
from dataclasses import dataclass


@dataclass
class SympyConstraint:
    lhs: Any
    rhs: Any
    # If it is self-recursive, if False, it can also be cyclic recursive
    isRec: bool = False

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __iter__(self):
        yield self.lhs
        yield self.rhs
