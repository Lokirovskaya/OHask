from typing import Tuple, List
import re

Item = Tuple[str, int]


def parseDynResult():
    with open("stat/dyn_result.txt", "r") as f:
        lines = f.readlines()

    for line in lines:
        groupIdx, inputItems, outputItems = parseLine(line)


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
    groups = re.findall(r"\((\w+),(\d+)\)", itemStr)

    def toItem(tup):
        return prefix + tup[0], int(tup[1])

    return list(map(toItem, groups))
