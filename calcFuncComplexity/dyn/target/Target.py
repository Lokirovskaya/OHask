from sympy import Expr
from typing import Dict
from calcFuncComplexity.struct import Constraint


class Target:
    _id = 0

    def __init__(self, expr: Expr, belongs: Constraint):
        self.expr = expr
        self.belongs = belongs
        self.id = Target._id
        Target._id += 1
        # List of real vars
        # The corresponding input var of a real var is the index of that in list
        # Input vars are vars requiring random test cases
        self._inputVars = []

    def __str__(self):
        return f"{self.expr} @{self.belongs.lhs.name}"

    # Create an input var
    def newInputVar(self, realVar: Expr):
        if realVar not in self._inputVars:
            self._inputVars.append(realVar)

    # Get the corresponding input var of real var
    # return: idx of the input var
    # ensure the result exists
    def getInputVar(self, realVar: Expr) -> int:
        return self._inputVars.index(realVar)

    def getRealVar(self, idx) -> Expr:
        return self._inputVars[idx]

    def inputVarLen(self) -> int:
        return len(self._inputVars)
