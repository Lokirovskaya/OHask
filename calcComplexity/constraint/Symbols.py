from calcComplexity.constraint import ExprInfo
import calcComplexity.haskellStruct as haskell
from calcComplexity.untypedLambdaCalculus import Var


def constant() -> Var:
    return Var("C")


def isConstant(var: Var) -> bool:
    return var.name == "C"


def add() -> Var:
    return Var("+", isValue=False)


def maxN() -> Var:
    return Var("maxN", isValue=False)


uuidUnique = 0
uuidExpr = 0


def unique() -> Var:
    global uuidUnique
    uuidUnique += 1
    return Var(f"u{uuidUnique}")


def complexity(name: str) -> Var:
    return Var(f"T_{name}", isValue=False)


class ExprSymbol(Var):
    def __init__(
        self, name: str, exprInfo: ExprInfo, isValue: bool = True, **kwargs
    ) -> None:
        super().__init__(name, isValue, **kwargs)
        self.exprInfo = exprInfo


def expr(expr: haskell.Expr) -> ExprSymbol:
    global uuidExpr
    uuidExpr += 1
    return ExprSymbol(f"e{uuidExpr}", isValue=False, exprInfo=ExprInfo(expr))
