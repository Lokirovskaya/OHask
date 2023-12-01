from typing import List
from ..struct import Constraint
from .target.FindTargets import findTargets


def runDynamicAnalysis(constrList: List[Constraint]):
    targets = findTargets(constrList)
