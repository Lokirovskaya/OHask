from sympy import Symbol
from typing import Any


class Constraint:
    def __init__(self, lhs: Symbol, rhs: Any):
        self.lhs = lhs
        self.rhs = rhs  # None means external symbol

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Constraint):
            return False
        return self.lhs == __value.lhs

    def __hash__(self) -> int:
        return hash(self.lhs)
