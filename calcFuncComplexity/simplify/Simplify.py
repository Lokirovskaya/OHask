from typing import List
from ..struct.Constraint import Constraint
from .ApplyBuiltinConstraints import applyBuiltinConstraints
from .SqueezeConstTerms import squeezeConstTerms
from .InlineConstFunctions import inlineConstFunctions
from .RemoveUnusedConstraints import removeUnusedConstraints


def simplifyConstraints(constrList: List[Constraint]):
    applyBuiltinConstraints(constrList)
    squeezeConstTerms(constrList)
    inlineConstFunctions(constrList)
    removeUnusedConstraints(constrList)
