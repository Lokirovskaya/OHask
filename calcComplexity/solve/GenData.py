from typing import Dict, List

import pandas as pd

from calcComplexity.solve.BasicFuncs import basicFuncs1, basicFuncs2, basicFuncs3
from calcComplexity.solve.Data import Data, RawData
from itertools import combinations

# {"v0": [], ...}
DictT = Dict[str, List[int]]


def genData(raw: RawData) -> Data:
    data = rawToData(raw)
    extendInputVars(data)
    print(data.inputExtData)
    return data


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
            for bfName, bfFunc in basicFuncs1.items():
                extName = col + "$" + bfName
                ext[extName] = origin.apply(lambda row: bfFunc(row[col]), axis=1)

    if len(cols) >= 2:
        for col1, col2 in combinations(cols, 2):
            for bfName, bfFunc in basicFuncs2.items():
                extName = col1 + "$" + col2 + "$" + bfName
                ext[extName] = origin.apply(lambda row: bfFunc(row[col1], row[col2]), axis=1)
