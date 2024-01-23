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


def expr(expr: haskell.Expr) -> Var:
    global uuidExpr
    uuidExpr += 1
    exprInfo = ExprInfo(expr)
    return Var(f"e{uuidExpr}", isValue=False, exprInfo=exprInfo)
