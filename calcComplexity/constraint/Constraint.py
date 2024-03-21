from typing import List
from calcComplexity.untypedLambdaCalculus import Var, Expr


class Constraint:
    def __init__(self, lhs: Var, lhsParams: List[Var], rhs: Expr) -> None:
        self.lhs = lhs
        self.lhsParams = lhsParams
        self.rhs = rhs

    def __str__(self) -> str:
        if len(self.lhsParams) == 0:
            return  f"{self.lhs} = {self.rhs}"
        else:
            paramStr = " ".join(map(str,self.lhsParams))
            return f"{self.lhs} {paramStr} = {self.rhs}"
