from dataclasses import dataclass

from calcComplexity.basicFuncs import BasicFunc

from .Lambda import Expr


@dataclass(repr=False)
class Term(Expr):
    coef: float
    basicFunc: BasicFunc

    def __str__(self) -> str:
        termStr = 
        if self.coef >= 0.0:
            return f"{self.coef} * {termStr}"
        return super().__str__()


@dataclass(repr=False)
class Const(Term):
    coef: float

    def __str__(self) -> str:
        return str(self.coef)
