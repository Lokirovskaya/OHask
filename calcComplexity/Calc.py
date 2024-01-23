from calcComplexity.genConstraints import buildStruct, genConstraints
from .Config import LOG_PATH


def calcComplexity(funcsData):
    with open(LOG_PATH, "w"):
        pass

    funcList = buildStruct(funcsData)
    constrList = genConstraints(funcList)
