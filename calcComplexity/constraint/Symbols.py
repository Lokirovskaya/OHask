from calcComplexity.constraint import ExprInfo
import calcComplexity.haskellStruct as haskell
from calcComplexity.untypedLambdaCalculus import Var


def constant() -> Var:
    return Var("C")


def isConstant(var: Var) -> bool:
    return var.name == "C"


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

    def __eq__(self, other) -> bool:
        if isinstance(other, ExprSymbol):
            # Each ExprSumbol holds a unique name
            return self.name == other.name
        else:
            return False

    def __hash__(self):
        return hash(self.name)


def expr(expr: haskell.Expr) -> ExprSymbol:
    global uuidExpr
    uuidExpr += 1
    return ExprSymbol(f"e{uuidExpr}", isValue=False, exprInfo=ExprInfo(expr))
