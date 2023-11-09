from typing import List

from .PromoteStatLambdas import promoteStatLambdas
from .SymbolizeVars import symbolize
from .GenConstrains import genConstrainList
from .Api import Func


def calcCompl(funcList: List[Func]):
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constrainList = genConstrainList(funcList, varsymDict)
    for lhs, rhs in constrainList:
        print(f"{lhs} = {rhs}")
