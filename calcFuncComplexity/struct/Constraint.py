from __future__ import annotations
from sympy import Function, Symbol, Expr, Lambda
from typing import Any, Optional


class Constraint:
    def __init__(self, lhs: Symbol | Function, rhs: Optional[Expr]):
        self.lhs = lhs
        self.rhs = rhs  # None means external symbol

    # Substitute all constr.lhs with constr.rhs in self.rhs
    def substitute(self, constr: Constraint):
        # if self.rhs == None:
            # return
        # self.rhs = Lambda(constr.lhs, self.rhs)(constr.rhs)
        pass

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Constraint):
            return False
        return self.lhs == __value.lhs

    def __hash__(self) -> int:
        return hash(self.lhs)
