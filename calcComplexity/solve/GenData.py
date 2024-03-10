from typing import Dict, List
from calcComplexity.solve import RawData, Data
import pandas as pd

# {"v0": [], ...}
DictT = Dict[str, List[int]]


def genData(raw: RawData) -> Data:
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
