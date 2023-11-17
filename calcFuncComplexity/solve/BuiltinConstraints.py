from sympy import Symbol
from typing import List, Tuple, Dict
from queue import SimpleQueue
from ..struct.Constraint import Constraint


def applyBuiltinConstraints(constrList: List[Constraint]):
    for constr in constrList:
        constr.substitute(Symbol('external'), 1)
