from __future__ import annotations
from sympy import Symbol
from typing import Any, List, Optional, Set, Dict

from ..preprocess.GenConstraints import Constraint
from ..preprocess.LazyLambda import LazyLambda, LazyApply, LazySubstitute, LazyAdd


class SymbolNode:
    def __init__(self, lhs: Symbol, rhs: Optional[Any]):
        self.lhs = lhs
        self.rhs = rhs  # None means external symbol
        self.depSymNodeSet: Set[SymbolNode] = set()

    def __str__(self) -> str:
        depSymNames = [node.lhs.name for node in self.depSymNodeSet]
        depSymStr = ", ".join(depSymNames)
        return f"{self.lhs} -> [{depSymStr}]"


def buildDepGraph(constraintList: List[Constraint]) -> Dict[Symbol, SymbolNode]:
    symNodeDict = {}

    # Add defined complexity symbol
    for cons in constraintList:
        lhs = cons.lhs
        rhs = cons.rhs
        assert lhs not in symNodeDict, f"Duplicated lhs {lhs}."
        symNode = SymbolNode(lhs, rhs)
        symNodeDict[lhs] = symNode

    # Add external complexity symbol
    # Fill the field depSymNodeSet of nodes
    for cons in constraintList:
        lhs = cons.lhs
        rhs = cons.rhs
        complSyms = findComplSymbols(rhs)
        for depSym in complSyms:
            # An external compl sym
            if depSym not in symNodeDict:
                symNodeDict[depSym] = SymbolNode(depSym, None)

            symNodeDict[lhs].depSymNodeSet.add(symNodeDict[depSym])

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
