from typing import List
from ..struct import Constraint
from .target.FindTargets import findTargets
from .target.GenTestFile import genTestFile


def runDynamicAnalysis(constrList: List[Constraint]):
    targets = findTargets(constrList)
    genTestFile(targets, "run/Dyn.hs")
