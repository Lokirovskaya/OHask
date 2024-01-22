from calcComplexity.untypedLambdaCalculus import Var, Expr


class Constraint:
    def __init__(self, lhs: Var, rhs: Expr) -> None:
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self) -> str:
        return str(self.lhs) + " == " + str(self.rhs)
