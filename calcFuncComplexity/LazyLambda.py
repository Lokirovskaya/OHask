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


# expr(arg)
class LazyApply(LazyExpr):
    def __init__(self, expr, arg):
        self.appExpr = expr
        self.appArg = arg


# expr[var -> newExpr]
class LazySubstitute(LazyExpr):
    # substList: [(var, newExpr), ...]
    # Please ensure each 2 substitutions are independent.
    def __init__(self, expr, substList: List[Tuple[sympy.Symbol, Any]]):
        self.substExpr = expr
        self.substList = substList
