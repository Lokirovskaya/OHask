from typing import Dict, List
from calcComplexity.solve import RawData, Data
import pandas as pd

# {"v0": [], ...}
DictT = Dict[str, List[int | None]]


def genData(raw: RawData) -> Data:
    assert len(raw.inputLines) == len(raw.outputLines)
    lineLen = len(raw.inputLines)

    inputDict: DictT = {name: ([None] * lineLen) for name in raw.inputVarNames}
    outputDict: DictT = {name: ([None] * lineLen) for name in raw.outputVarNames}

    for i in range(lineLen):
        inputs = raw.inputLines[i]
        outputs = raw.outputLines[i]
        for name, val in inputs:
            inputDict[name][i] = val  # Guarentee `name in inputDict`
        for name, val in outputs:
            outputDict[name][i] = val

    inputDf = pd.DataFrame(inputDict)
    outputDf = pd.DataFrame(outputDict)
    
    return Data(raw.groupIdx, inputDf, outputDf)
