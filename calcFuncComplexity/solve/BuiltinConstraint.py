from sympy import Symbol
from typing import List, Tuple, Dict
from queue import SimpleQueue
from ..struct.Constraint import Constraint
from .DependencyGraph import SymbolNode

def applyBuiltinConstraint(constrList: List[Constraint]):
    for constr in constrList:
        if constr.rhs == None:
            constr.rhs = 1
