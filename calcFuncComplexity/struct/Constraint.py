from __future__ import annotations
from sympy import Function, Symbol, Expr
from typing import Any, Optional


class Constraint:
    def __init__(self, lhs: Symbol | Function, rhs: Optional[Expr]):
        self.lhs = lhs
        self.rhs = rhs

    # Substitute all `old` with `new` in `self.rhs`
    def substitute(self, old, new):
        if self.rhs == None:
            return
        if hasattr(self.rhs, "replace"):
            self.rhs = self.rhs.replace(old, new)
        pass

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Constraint):
            return False
        return self.lhs == __value.lhs

    def __hash__(self) -> int:
        return hash(self.lhs)
