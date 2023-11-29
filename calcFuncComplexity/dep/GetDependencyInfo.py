from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.GenConstraints import genConstraintList
from .simplify.Simplify import simplifyConstraints


def getDependencyInfo(funcListData):
    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    constrList = genConstraintList(funcList)
    simplifyConstraints(constrList)

    return constrList
