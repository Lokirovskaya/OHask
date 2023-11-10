from typing import List

from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.SymbolizeVars import symbolize
from .preprocess.GenConstrains import genConstrainList

from .solve.DependencyGraph import buildDepGraph


def calcCompl(funcListData):
    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constrainList = genConstrainList(funcList, varsymDict)

    for lhs, rhs in constrainList:
        print(f"{lhs} = {rhs}\n")

    symNodeDict = buildDepGraph(constrainList)
    
    for _, node in symNodeDict.items():
        print(node)
