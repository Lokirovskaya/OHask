from typing import Any, List
from sympy import Symbol


class SympyConstraint:
    def __init__(self, lhs: Symbol, lhsParams: List[Any], rhs: Any) -> None:
        self.lhs = lhs
        self.lhsParams = lhsParams
        self.rhs = rhs

    def __str__(self) -> str:
        if len(self.lhsParams) == 0:
            return f"{self.lhs} = {self.rhs}"
        else:
            paramStr = " ".join(map(str, self.lhsParams))
            return f"{self.lhs} {paramStr} = {self.rhs}"
