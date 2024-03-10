from calcComplexity.genConstraints import buildStruct, genConstraints
from calcComplexity.runDynExec import makeGroups, genHaskellProgram, runRepl
from calcComplexity.regression import parseDynResult, genData

RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
BOLD = "\x1b[1m"
END = "\x1b[0m"


def calcComplexity(runDyn: bool, runSolve: bool):

    # =========================================================== #

    print(f"{BOLD}{YELLOW}=== Loading Program Info ==={END}")

    funcList = buildStruct()
    constrList, exprSymbolList = genConstraints(funcList)
    groupList = makeGroups(exprSymbolList)

    print(f"{GREEN}Success{END}\n")

    # =========================================================== #

    if runDyn:
        print(f"{BOLD}{YELLOW}=== Running Dynamic Analysis ==={END}")

        genHaskellProgram(funcList, groupList)
        runRepl(groupList)

        print(f"{GREEN}Success{END}\n")

    # =========================================================== #

    if runSolve:
        print(f"{BOLD}{YELLOW}=== Solving Complexity ==={END}")

        rawDatas = parseDynResult()
        datas = map(genData, rawDatas)
        
        print(f"{GREEN}Success{END}\n")