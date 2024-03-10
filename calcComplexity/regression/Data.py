from dataclasses import dataclass, field
from typing import List, Set, Tuple

import pandas as pd

Item = Tuple[str, int]

RawLine = List[Item]


@dataclass
class RawData:
    groupIdx: int
    inputVarNames: Set[str] = field(default_factory=set)
    inputLines: List[RawLine] = field(default_factory=list)
    outputVarNames: Set[str] = field(default_factory=set)
    outputLines: List[RawLine] = field(default_factory=list)


@dataclass
class Data:
    groupIdx: int
    # Data lines: test cases
    # Data cols: vars
    inputData: pd.DataFrame
    outputData: pd.DataFrame
    # Filled in GenData/extendInputVars
    inputExtData: pd.DataFrame = field(default_factory=pd.DataFrame)
