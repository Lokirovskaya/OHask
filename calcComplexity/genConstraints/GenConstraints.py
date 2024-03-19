from typing import Dict, List, Tuple

from calcComplexity.Log import logln
from calcComplexity.constraint import Constraint, ExprSymbol
import calcComplexity.constraint.Symbols as symbol
from calcComplexity.haskellStruct import App, Case, Expr, Func, Lit, Var
import calcComplexity.untypedLambdaCalculus as lam

from .FillDepAndDef import fillDepAndDef
from .Simplify import simplify

exprSymbolList: List[ExprSymbol] = []


def genConstraints(
    funcList: List[Func],
) -> Tuple[List[Constraint], List[ExprSymbol], Dict[Var, lam.Var]]:

    constrList = []
    paramH2LTable = {}  # Haskell param => Constr param (lambda param) table

    for func in funcList:
        # Constraint: T == \ps. T'
        # `ps` represents lambda params

        lhs = symbol.complexity(func.funcUnique)  # T

        compl = calcExprCompl(func.funcExpr)  # T'
        paramLs = []  # ps
        for idx, paramH in enumerate(func.funcParams):
            paramL = symbol.param(func.funcUnique, idx)
            paramLs.append(paramL)
            paramH2LTable[paramH] = paramL

        rhs = lam.currying(paramLs, compl)

        constr = Constraint(lhs, rhs)
        constrList.append(constr)

    logln("[Constraints]")
    for constr in constrList:
        logln(str(constr))
    logln()

    logln("[Param Lookup Table]")
    for h, l in paramH2LTable.items():
        logln(f"{h} -> {l}")
    logln()

    fillDepAndDef(funcList, exprSymbolList)

    simplify(constrList, exprSymbolList)

    return constrList, exprSymbolList, paramH2LTable


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
        # uniqueParams = [symbol.unique() for _ in range(0, arity)]
        # compl = symbol.complexity(var.varUnique)
        # return lam.currying(uniqueParams, compl)
        compl = symbol.complexity(var.varUnique)
        return compl


def calcLitCompl(lit: Lit) -> lam.Expr:
    return symbol.constant()


def calcAppCompl(app: App) -> lam.Expr:
    # For E(A)
    # Compl = C + T_A + T_E(e_A)
    exprCompl = calcExprCompl(app.appExpr)
    argCompl = calcExprCompl(app.appArg)
    argExprSymbol = symbol.expr(app.appArg)
    exprSymbolList.append(argExprSymbol)  # Record all expr symbols
    return lam.Sum([symbol.constant(), argCompl, exprCompl(argExprSymbol)])


def calcCaseCompl(case_: Case) -> lam.Expr:
    caseExprCompl = calcExprCompl(case_.caseExpr)
    if case_.caseAltCount == 0:
        return caseExprCompl
    else:
        altsCompl = [calcExprCompl(alt.altExpr) for alt in case_.caseAlts]
        return lam.Sum([caseExprCompl, lam.MaxN(altsCompl)])
