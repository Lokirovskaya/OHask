# pyright: reportPossiblyUnboundVariable=false

from typing import Tuple, List
import re
from calcComplexity.regression.Data import Item, RawData, RawLine


def parseDynResult() -> List[RawData]:
    with open("stat/dyn_result.txt", "r") as f:
        lines = f.readlines()

    rawDatas: List[RawData] = []
    curGroup = -1

    for line in lines:
        groupIdx, inputItems, outputItems = parseLine(line)
        # New group or first group
        if groupIdx != curGroup:
            curGroup = groupIdx
            curData = RawData(groupIdx)
            rawDatas.append(curData)

        curData.inputLines.append(inputItems)
        curData.outputLines.append(outputItems)
        for name, _ in inputItems:
            curData.inputVarNames.add(name)
        for name, _ in outputItems:
            curData.outputVarNames.add(name)

    return rawDatas


def parseLine(line: str) -> Tuple[int, List[Item], List[Item]]:
    inputItems: List[Item] = []
    outputItems: List[Item] = []

    s = line.split()
    assert s[0] == "$"
    groupIdx = int(s[1])
    assert s[2] == "<"

    # inputs
    i = 3
    inputIdx = 0
    while s[i] != ">":
        inputItems += parseItemList(s[i], prefix=("i" + str(inputIdx)))
        i += 1
        inputIdx += 1

    i += 1

    # outputs
    outputIdx = 0
    while s[i] != "^":
        outputItems += parseItemList(s[i], prefix=("o" + str(outputIdx)))
        i += 1
        outputIdx += 1

    return groupIdx, inputItems, outputItems


def parseItemList(itemStr: str, prefix: str) -> List[Item]:
    if itemStr == "[]":
        return []

    # [(ValType,Value)]
    groups = re.findall(r"\((\w+),(-?\d+)\)", itemStr)
    assert len(groups) > 0, f"Nothing matches in '{itemStr}'"

    def toItem(tup):
        return prefix + tup[0], int(tup[1])

    return list(map(toItem, groups))
