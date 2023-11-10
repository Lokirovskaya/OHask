from __future__ import annotations
from typing import List, Tuple, Any

import sympy


# Untyped lambda calculus
class LazyExpr:
    pass


# λ param. expr
class LazyLambda(LazyExpr):
    def __init__(self, param: sympy.Symbol, expr):
        self.lamParam = param
        self.lamExpr = expr

    def __str__(self) -> str:
        return f"λ{self.lamParam}. {self.lamExpr}"


# expr(arg)
class LazyApply(LazyExpr):
    def __init__(self, expr, arg):
        self.appExpr = expr
        self.appArg = arg

    def __str__(self) -> str:
        return f"({self.appExpr})({self.appArg})"


# expr[var -> newExpr]
class LazySubstitute(LazyExpr):
    # substList: [(var, newExpr), ...]
    # Please ensure each 2 substitutions are independent.
    def __init__(self, expr, substList: List[Tuple[sympy.Symbol, Any]]):
        self.substExpr = expr
        self.substList = substList

    def __str__(self) -> str:
        substStrList = [f"{old}->{new}" for (old, new) in self.substList]
        return f"{self.substExpr}[{', '.join(substStrList)}]"


# lhs + rhs
class LazyAdd(LazyExpr):
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self) -> str:
        return f"({self.lhs}) + ({self.rhs})"
