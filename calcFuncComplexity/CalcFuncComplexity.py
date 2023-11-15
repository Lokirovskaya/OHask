from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.GenConstraints import genConstraintList

from .solve.BuildDepGraph import buildDepGraph
from .solve.TopoSort import topoSortDepGraph
from .solve.BuiltinConstraints import applyBuiltinConstraints
from .solve.Solve import solve


def calcCompl(funcListData):
    newLog()

    funcList = [Func(funcData) for funcData in funcListData]
    promoteStatLambdas(funcList)
    constraintList = genConstraintList(funcList)
    log("[Raw Constraints]")
    for con in constraintList:
        log(con)

    depNodeDict = buildDepGraph(constraintList)
    log("\n[Symbol Dependency Info]")
    for _, node in depNodeDict.items():
        log(node)

    (reductionSeq, recConstrList) = topoSortDepGraph(depNodeDict)
    applyBuiltinConstraints(reductionSeq)
    applyBuiltinConstraints(recConstrList)
    log("\n[Reduction Sequence]")
    for con in reductionSeq:
        log(con.lhs)
    log("\n[Recursive Constraints]")
    for con in recConstrList:
        log(con.lhs)

    solveResult = solve(reductionSeq, recConstrList)
    log("\n[Solve Result]")
    for con in solveResult:
        log(con)


logFile = "stat/calc_log.txt"


def newLog():
    with open(logFile, "w"):
        pass


def log(s):
    with open(logFile, "a") as f:
        f.write(str(s) + "\n")
