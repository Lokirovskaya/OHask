from sympy import Expr
from ...struct import Constraint


class Target:
    _id = 0

    def __init__(self, expr: Expr, belongs: Constraint):
        self.expr = expr
        self.belongs = belongs
        self.id = Target._id
        Target._id += 1

    def __str__(self):
        return f"{self.expr} @{self.belongs.lhs.name}"
