from calcComplexity.genConstraints import buildStruct, genConstraints
from calcComplexity.runDynExec import makeGroups, genHaskellProgram


def calcComplexity(funcsData):
    funcList = buildStruct(funcsData)
    constrList, exprSymbolList = genConstraints(funcList)

    groupList = makeGroups(exprSymbolList)
    genHaskellProgram(groupList)
