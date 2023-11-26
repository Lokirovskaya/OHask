from typing import List
from ..struct.Constraint import Constraint
from .BuiltinConstraints import applyBuiltinConstraints
from .RemoveConstTerms import removeConstTerms


def simplifyConstraints(constrList: List[Constraint]):
    applyBuiltinConstraints(constrList)
    removeConstTerms(constrList)
