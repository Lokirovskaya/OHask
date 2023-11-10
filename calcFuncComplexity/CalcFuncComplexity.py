from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.SymbolizeVars import symbolize
from .preprocess.GenConstraints import genConstraintList

from .solve.DependencyGraph import buildDepGraph
from .solve.TopoSort import topoSortDepGraph


def calcCompl(funcListData):
    newLog()

    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constraintList = genConstraintList(funcList, varsymDict)

    log('Raw Constraints')
    for con in constraintList:
        log(con)

    symNodeDict = buildDepGraph(constraintList)
    topoSeq = topoSortDepGraph(symNodeDict)

    log('\nSymbol Dependency Info')
    for _, node in symNodeDict.items():
        log(node)

    log('\nReduction Sequence')
    for con in topoSeq:
        log(con)


logFile = "stat/calc_log.txt"


def newLog():
    with open(logFile, "w"):
        pass


def log(s):
    with open(logFile, "a") as f:
        f.write(str(s) + "\n")
