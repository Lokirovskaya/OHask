from calcComplexity.genConstraints import buildStruct, genConstraints


def calcComplexity(funcsData):
    funcList = buildStruct(funcsData)
    constrList = genConstraints(funcList)
