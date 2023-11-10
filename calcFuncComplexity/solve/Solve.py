from sympy import Symbol
from typing import List, Tuple, Dict
from queue import SimpleQueue

from ..struct.LazyLambda import LazyLambda, LazyAdd, LazySubstitute, LazyApply
from ..struct.Constraint import Constraint
from .DependencyGraph import SymbolNode


def solve(
    reductionSeq: List[Constraint], recConstrList: List[Constraint]
) -> List[Constraint]:
    # Substitution
    for i in range(len(reductionSeq) - 1):
        for j in range(i, len(reductionSeq)):
            reductionSeq[j].substitute(reductionSeq[i])

    for recConstr in recConstrList:
        for reduction in reductionSeq:
            recConstr.substitute(reduction)

    # Evaluation of lazy lambdas

    return reductionSeq + recConstrList


def evalLazy(expr):
    if isinstance(expr, LazyLambda):
        if expr.lamExpr == constr.lhs:
            expr.lamExpr = constr.rhs
        else:
            runExpr(expr.lamExpr)

    elif isinstance(expr, LazyApply):
        if expr.appExpr == constr.lhs:
            expr.appExpr = constr.rhs
        else:
            runExpr(expr.appExpr)
        if expr.appArg == constr.lhs:
            expr.appArg = constr.rhs
        else:
            runExpr(expr.appArg)

    elif isinstance(expr, LazySubstitute):
        if expr.substExpr == constr.lhs:
            expr.substExpr = constr.rhs
        else:
            runExpr(expr.substExpr)

    elif isinstance(expr, LazyAdd):
        if expr.lhs == constr.lhs:
            expr.lhs = constr.rhs
        else:
            runExpr(expr.lhs)
        if expr.rhs == constr.lhs:
            expr.rhs = constr.rhs
        else:
            runExpr(expr.rhs)
