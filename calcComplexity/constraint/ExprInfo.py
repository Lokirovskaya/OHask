from typing import Set

import calcComplexity.haskellStruct as haskell
from calcComplexity.haskellStruct import getAllVars


class ExprInfo:
    def __init__(self, expr: haskell.Expr) -> None:
        self.expr = expr
        # All vars occurred in expr
        self.varSet: Set[haskell.Var] = getAllVars(expr)
        # Vars that the expr depends on
        # Note: Most of var dependencies are not listed here.
        #       We only take into account critical vars such as function params.
        self.dependsOnCrit: Set[haskell.Var] = set()

    def __str__(self) -> str:
        return f"expr={self.expr}, varSet={self.varSet}, dependsOn={self.dependsOnCrit}"
