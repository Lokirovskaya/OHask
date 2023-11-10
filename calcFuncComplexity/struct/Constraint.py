from __future__ import annotations
from sympy import Symbol
from typing import Any
from ..struct.LazyLambda import LazyLambda, LazyApply, LazyAdd, LazySubstitute


class Constraint:
    def __init__(self, lhs: Symbol, rhs: Any):
        self.lhs = lhs
        self.rhs = rhs  # None means external symbol

    # Substitute all constr.lhs with constr.rhs in self.rhs
    def substitute(self, constr: Constraint):
        def runExpr(expr):
            if isinstance(expr, LazyLambda):
                if expr.lamExpr == constr.lhs:
                    expr.lamExpr = constr.rhs
                else:
                    runExpr(expr.lamExpr)

            elif isinstance(expr, LazyApply):
                if expr.appExpr == constr.lhs:
                    expr.appExpr = constr.rhs
                else:
                    runExpr(expr.appExpr)
                if expr.appArg == constr.lhs:
                    expr.appArg = constr.rhs
                else:
                    runExpr(expr.appArg)

            elif isinstance(expr, LazySubstitute):
                if expr.substExpr == constr.lhs:
                    expr.substExpr = constr.rhs
                else:
                    runExpr(expr.substExpr)

            elif isinstance(expr, LazyAdd):
                if expr.lhs == constr.lhs:
                    expr.lhs = constr.rhs
                else:
                    runExpr(expr.lhs)
                if expr.rhs == constr.lhs:
                    expr.rhs = constr.rhs
                else:
                    runExpr(expr.rhs)

        runExpr(self.rhs)

    def __str__(self) -> str:
        return f"{self.lhs} = {self.rhs}"

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Constraint):
            return False
        return self.lhs == __value.lhs

    def __hash__(self) -> int:
        return hash(self.lhs)
