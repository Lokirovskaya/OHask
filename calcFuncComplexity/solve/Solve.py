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