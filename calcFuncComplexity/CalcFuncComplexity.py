from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.SymbolizeVars import symbolize
from .preprocess.GenConstraints import genConstraintList

from .solve.DependencyGraph import buildDepGraph
from .solve.TopoSort import topoSortDepGraph
from .solve.BuiltinConstraint import applyBuiltinConstraint
from .solve.Solve import solve


def calcCompl(funcListData):
    newLog()

    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    varsymDict = symbolize(funcList)
    constraintList = genConstraintList(funcList, varsymDict)
    log("Raw Constraints")
    for con in constraintList:
        log(con)

    symNodeDict = buildDepGraph(constraintList)
    log("\nSymbol Dependency Info")
    for _, node in symNodeDict.items():
        log(node)

    (reductionSeq, recConstrList) = topoSortDepGraph(symNodeDict)
    applyBuiltinConstraint(reductionSeq)
    applyBuiltinConstraint(recConstrList)
    log("\nReduction Sequence")
    for con in reductionSeq:
        log(con.lhs)
    log("\nRecursive Constraints")
    for con in recConstrList:
        log(con.lhs)

    solveResult = solve(reductionSeq, recConstrList)
    log("\nSolve Result")
    for con in solveResult:
        log(con)


logFile = "stat/calc_log.txt"


def newLog():
    with open(logFile, "w"):
        pass


def log(s):
    with open(logFile, "a") as f:
        f.write(str(s) + "\n")
