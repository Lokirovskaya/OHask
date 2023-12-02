from sympy import Expr
from ...struct import Constraint


class Target:
    def __init__(self, expr: Expr, belongs: Constraint):
        self.expr = expr
        self.belongs = belongs

    def __str__(self):
        return f"{self.expr} @{self.belongs.lhs.name}"
