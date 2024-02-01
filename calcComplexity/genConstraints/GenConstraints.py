from typing import List, Tuple

from calcComplexity.Config import LOG_PATH
from calcComplexity.constraint import Constraint, ExprSymbol
import calcComplexity.constraint.Symbols as symbol
from calcComplexity.haskellStruct import App, Case, Expr, Func, Lit, Var
import calcComplexity.untypedLambdaCalculus as lam

from .VarDep import findVarDep

exprSymbolList: List[ExprSymbol] = []


def genConstraints(funcList: List[Func]) -> Tuple[List[Constraint], List[ExprSymbol]]:
    constrList = []
    for func in funcList:
        complSymbol = symbol.complexity(func.funcUnique)
        compl = calcExprCompl(func.funcExpr)
        constr = Constraint(complSymbol, compl)
        constrList.append(constr)

    with open(LOG_PATH, "a") as f:
        f.write("[Constraints]\n")
        for constr in constrList:
            f.write(str(constr) + "\n")
        f.write("\n")

    findVarDep(funcList, exprSymbolList)

    return constrList, exprSymbolList


def calcExprCompl(expr: Expr) -> lam.Expr:
    if isinstance(expr, Var):
        return calcVarCompl(expr)
    elif isinstance(expr, Lit):
        return calcLitCompl(expr)
    elif isinstance(expr, App):
        return calcAppCompl(expr)
    elif isinstance(expr, Case):
        return calcCaseCompl(expr)
    else:
        assert False


def calcVarCompl(var: Var) -> lam.Expr:
    arity = var.varArity
    if arity == 0:
        return symbol.constant()
    else:
        # Complexity of var `f` is an lambda: `λp1. ... λpn. T(p1,...,pn)`
        uniqueParams = [symbol.unique() for _ in range(0, arity)]
        compl = symbol.complexity(var.varUnique)
        return lam.currying(uniqueParams, compl)


def calcLitCompl(lit: Lit) -> lam.Expr:
    return symbol.constant()


def calcAppCompl(app: App) -> lam.Expr:
    # For E(A)
    # Compl = C + T_A + T_E(e_A)
    exprCompl = calcExprCompl(app.appExpr)
    argCompl = calcExprCompl(app.appArg)
    argExprSymbol = symbol.expr(app.appArg)
    exprSymbolList.append(argExprSymbol)  # Record all expr symbols
    return add(symbol.constant(), add(argCompl, exprCompl(argExprSymbol)))


def calcCaseCompl(case_: Case) -> lam.Expr:
    caseExprCompl = calcExprCompl(case_.caseExpr)
    if case_.caseAltCount == 0:
        return caseExprCompl
    else:
        altsCompl = [calcExprCompl(alt.altExpr) for alt in case_.caseAlts]
        return add(caseExprCompl, symbol.maxN()(*altsCompl))


def add(x: lam.Expr, y: lam.Expr) -> lam.Expr:
    return symbol.add()(x, y)
