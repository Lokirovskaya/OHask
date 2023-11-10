from typing import List

from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.SymbolizeVars import symbolize
from .preprocess.GenConstraints import genConstraintList

from .solve.DependencyGraph import buildDepGraph


def calcCompl(funcListData):
    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constraintList = genConstraintList(funcList, varsymDict)

    for con in constraintList:
        print(con)

    symNodeDict = buildDepGraph(constraintList)

    for _, node in symNodeDict.items():
        print(node)
