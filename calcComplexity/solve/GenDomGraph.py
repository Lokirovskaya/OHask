from typing import List

from calcComplexity.constraint import Constraint
import calcComplexity.untypedLambdaCalculus as lam
import calcComplexity.constraint.Symbols as syms
from .FuncDom import addDomEdge, outputGraph


def genDomGraph(constrList: List[Constraint]):
    for constr in constrList:
        graphV = constr.lhs.name
        allVars = lam.getAllVars(constr.rhs)
        for var in allVars:
            if syms.isComplexity(var):
                graphU = var.name
                addDomEdge(graphU, graphV)
    
    outputGraph()
