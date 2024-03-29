from itertools import combinations
from typing import Dict, List

import pandas as pd

from calcComplexity.basicFuncs import (
    unaryBasicFuncs,
    binaryBasicFuncs,
    ternaryBasicFuncs,
)

from .Data import Data, RawData


def genDatas(raws: List[RawData]) -> List[Data]:
    return list(map(genData, raws))


def genData(raw: RawData) -> Data:
    data = rawToData(raw)
    extendInputVars(data)
    return data


# {"v0": [], ...}
DictT = Dict[str, List[int]]


def rawToData(raw: RawData) -> Data:
    assert len(raw.inputLines) == len(raw.outputLines)
    lineLen = len(raw.inputLines)

    inputDict: DictT = {name: [] for name in raw.inputVarNames}
    outputDict: DictT = {name: [] for name in raw.outputVarNames}

    for i in range(lineLen):
        inputs = raw.inputLines[i]
        outputs = raw.outputLines[i]
        # If len of input/output < len of corresponding name list,
        # it means some data are missing, ignore current line.
        if len(inputs) < len(raw.inputVarNames) or (
            len(outputs) < len(raw.outputVarNames)
        ):
            continue

        for name, val in inputs:
            inputDict[name].append(val)  # Guarentee `name in inputDict`
        for name, val in outputs:
            outputDict[name].append(val)

    inputDf = pd.DataFrame(inputDict)
    outputDf = pd.DataFrame(outputDict)

    return Data(raw.groupIdx, inputDf, outputDf)


# Fill field `inputExtData`
def extendInputVars(data: Data):
    origin = data.inputData
    ext = data.inputExtData
    cols = origin.columns.to_list()  # Names of input vars

    if len(cols) >= 1:
        for col in cols:
            for func in unaryBasicFuncs:
                extName = col + "$" + func.name
                ext[extName] = origin.apply(lambda row: func.lambda_(row[col]), axis=1)

    if len(cols) >= 2:
        for col1, col2 in combinations(cols, 2):
            for func in binaryBasicFuncs:
                extName = col1 + "$" + col2 + "$" + func.name
                ext[extName] = origin.apply(
                    lambda row: func.lambda_(row[col1], row[col2]), axis=1
                )

    if len(cols) >= 3:
        for col1, col2, col3 in combinations(cols, 3):
            for func in binaryBasicFuncs:
                extName = col1 + "$" + col2 + "$" + col3 + "$" + func.name
                ext[extName] = origin.apply(
                    lambda row: func.lambda_(row[col1], row[col2], row[col3]), axis=1
                )
