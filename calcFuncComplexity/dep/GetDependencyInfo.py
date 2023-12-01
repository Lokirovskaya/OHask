from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.GenConstraints import genConstraintList
from .simplify.Simplify import simplifyConstraints
from ..struct import Constraint
from typing import List


def getDependencyInfo(funcListData) -> List[Constraint]:
    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    constrList = genConstraintList(funcList)
    simplifyConstraints(constrList)

    return constrList
