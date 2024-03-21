from typing import List

from calcComplexity.constraint import Constraint
from calcComplexity.regression import Data, LinearResult
import calcComplexity.untypedLambdaCalculus as lam
import calcComplexity.constraint.Symbols as syms


def makeScaleFuncs(
    constrList: List[Constraint], data: List[Data], regressionResult: List[LinearResult]
):
    
    for constr in constrList:
        pass
