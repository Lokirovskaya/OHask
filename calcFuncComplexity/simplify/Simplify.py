from typing import List
from ..struct.Constraint import Constraint
from .ApplyBuiltinConstraints import applyBuiltinConstraints
from .RemoveConstTerms import removeConstTerms
from .InlineConstFunctions import inlineConstFunctions


def simplifyConstraints(constrList: List[Constraint]):
    applyBuiltinConstraints(constrList)
    removeConstTerms(constrList)
    inlineConstFunctions(constrList)
