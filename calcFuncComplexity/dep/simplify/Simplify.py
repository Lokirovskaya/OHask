from typing import List
from ...struct import Constraint
from .ApplyBuiltinConstraints import applyBuiltinConstraints
from .SqueezeConstTerms import squeezeConstTerms
from .InlineConstFunctions import inlineConstFunctions
from ...Log import log


def simplifyConstraints(constrList: List[Constraint]):
    applyBuiltinConstraints(constrList)
    squeezeConstTerms(constrList)
    inlineConstFunctions(constrList)

    log("[Simplified Constraints]")
    for con in constrList:
        log(con)
    log()
