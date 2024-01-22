from typing import Sequence
from .Lambda import *


def currying(varList: Sequence[Var], expr: Expr) -> Expr:
    if len(varList) == 0:
        return expr
    elif len(varList) == 1:
        return Abstr(varList[0], expr)
    else:
        cur = Abstr(varList[-1], expr)
        for var in reversed(varList[:-1]):
            cur = Abstr(var, cur)
        return cur
        
