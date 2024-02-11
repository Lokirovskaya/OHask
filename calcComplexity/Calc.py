from calcComplexity.genConstraints import buildStruct, genConstraints
from calcComplexity.runDynExec import makeGroups
from .Config import LOG_PATH


def calcComplexity(funcsData):
    with open(LOG_PATH, "w"):
        pass

    funcList = buildStruct(funcsData)
    constrList, exprSymbolList = genConstraints(funcList)

    groupList = makeGroups(exprSymbolList)
