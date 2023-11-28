from typing import List
from ..struct.Constraint import Constraint
from sympy import Symbol, Function
from sympy.core import preorder_traversal
from sympy.core.function import UndefinedFunction


def removeUnusedConstraints(constrList: List[Constraint]):
    complOcurred = set()
    
    for constr in constrList:
        compls = [
            term.func
            for term in preorder_traversal(constr.rhs)
            if isComplFunc(term)
        ]
        print(compls)


def isComplFunc(s):
    return isinstance(s, (UndefinedFunction, Function)) and s.name.startswith("T")
