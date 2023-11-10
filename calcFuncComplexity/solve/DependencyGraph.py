from __future__ import annotations
from sympy import Symbol
from typing import Any, List, Optional, Set, Dict

from ..struct.Constraint import Constraint
from ..struct.LazyLambda import LazyLambda, LazyApply, LazySubstitute, LazyAdd


class SymbolNode:
    def __init__(self, constraint: Constraint):
        self.constraint = constraint
        self.depSymNodeSet: Set[SymbolNode] = set()
        # Update when running topo-sort
        self.childLeft: int = 0
        self.parentSet: Set[SymbolNode] = set()

    def __str__(self) -> str:
        depSymNames = [node.constraint.lhs.name for node in self.depSymNodeSet]
        depSymStr = ", ".join(depSymNames)
        return f"{self.constraint.lhs} -> [{depSymStr}]"

    def addDepSymNode(self, node: SymbolNode):
        self.depSymNodeSet.add(node)
        self.childLeft += 1
        node.parentSet.add(self)


def buildDepGraph(constraintList: List[Constraint]) -> Dict[Symbol, SymbolNode]:
    symNodeDict = {}

    # Add defined complexity symbol
    for cons in constraintList:
        assert cons.lhs not in symNodeDict, f"Duplicated lhs {cons.lhs}."
        symNode = SymbolNode(cons)
        symNodeDict[cons.lhs] = symNode

    # Add external complexity symbol
    # Fill the field depSymNodeSet of nodes
    for cons in constraintList:
        lhs = cons.lhs
        rhs = cons.rhs
        complSyms = findComplSymbols(rhs)
        for depSym in complSyms:
            # An external compl sym
            if depSym not in symNodeDict:
                symNodeDict[depSym] = SymbolNode(Constraint(depSym, None))

            symNodeDict[lhs].addDepSymNode(symNodeDict[depSym])

    return symNodeDict


def findComplSymbols(expr):
    complSymSet = set()

    def runExpr(expr):
        if isinstance(expr, Symbol) and expr.name.startswith("O_"):
            complSymSet.add(expr)

        # Complexity Symbols only occur at following fields
        if isinstance(expr, LazyLambda):
            runExpr(expr.lamExpr)
        elif isinstance(expr, LazyApply):
            runExpr(expr.appExpr)
            runExpr(expr.appArg)
        elif isinstance(expr, LazySubstitute):
            runExpr(expr.substExpr)
        elif isinstance(expr, LazyAdd):
            runExpr(expr.lhs)
            runExpr(expr.rhs)

    runExpr(expr)
    return complSymSet
