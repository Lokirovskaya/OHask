from .preprocess.Api import Func
from .preprocess.PromoteStatLambdas import promoteStatLambdas
from .preprocess.GenConstraints import genConstraintList

from .simplify.Simplify import simplifyConstraints

from .solve.BuildDepGraph import buildDepGraph
from .solve.TopoSort import topoSortDepGraph
from .solve.Solve import solve


def calcCompl(funcListData):
    newLog()

    funcList = [Func(funcData) for funcData in funcListData]

    solveResult = runSolve(funcList)


def runSolve(funcList):
    promoteStatLambdas(funcList)

    log("[Raw Constraints]")
    constraintList = genConstraintList(funcList)
    for con in constraintList:
        log(con)

    log("\n[Simplified Constraints]")
    simplifyConstraints(constraintList)
    for con in constraintList:
        log(con)

    log("\n[Complexity Dependency Info]")
    depNodeDict = buildDepGraph(constraintList)
    for _, node in depNodeDict.items():
        log(node)

    log("\n[Reduction Sequence]")
    (reductionSeq, recConstrList) = topoSortDepGraph(depNodeDict)
    for con in reductionSeq:
        log(con.lhs)
    log("\n[Recursive Constraints]")
    for con in recConstrList:
        log(con.lhs)

    log("\n[Solve Result]")
    solveResult = solve(reductionSeq, recConstrList)
    for con in solveResult:
        log(con)

    return solveResult


logFile = "stat/calc_log.txt"


def newLog():
    with open(logFile, "w"):
        pass


def log(s):
    with open(logFile, "a") as f:
        f.write(str(s) + "\n")
