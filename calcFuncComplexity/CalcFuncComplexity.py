from typing import List

from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.SymbolizeVars import symbolize
from .preprocess.GenConstrains import genConstrainList


def calcCompl(funcListData):
    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constrainList = genConstrainList(funcList, varsymDict)
    for lhs, rhs in constrainList:
        print(f"{lhs} = {rhs}")
