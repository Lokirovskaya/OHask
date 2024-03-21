from calcComplexity.constraint import ExprInfo
import calcComplexity.haskellStruct as haskell
from calcComplexity.untypedLambdaCalculus import Var, Expr

uuidUnique = 0
uuidExpr = 0


def constant() -> Var:
    return Var("C")


def isConstant(var: Expr) -> bool:
    return isinstance(var, Var) and var.isValue and var.name == "C"


def unique() -> Var:
    global uuidUnique
    uuidUnique += 1
    return Var(f"u{uuidUnique}")


def param(name: str, idx: int) -> Var:
    return Var(f"p{idx}_{name}")


def complexity(name: str, arity:int ) -> Var:
    return Var(f"T_{name}", isValue=False, arity=arity)


def isComplexity(var: Expr) -> bool:
    return isinstance(var, Var) and not var.isValue and var.name.startswith("T_")


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
