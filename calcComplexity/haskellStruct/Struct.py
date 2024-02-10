# See OHask/funcInfoStat/gen/StatInfo.hs

from __future__ import annotations
from typing import List, Optional, Set


class Func:
    def __init__(
        self,
        funcName: str,
        funcType: str,
        funcUnique: str,
        funcParentUnique: Optional[str],
        funcParams: List[Var],
        funcExpr: Expr,
    ) -> None:
        self.funcName = funcName
        self.funcType = funcType
        self.funcUnique = funcUnique
        self.funcParentUnique = funcParentUnique
        self.funcParams = funcParams
        self.funcParamCount = len(funcParams)
        self.funcExpr = funcExpr
        # to be filled in genDependencies/BuildStruct.py
        self.funcParent: Optional[Func] = None
        # var-like function
        self.varLike = Var(
            varName=funcName,
            varType=funcType,
            varModule=None,  # todo: precise module detect
            varUnique=funcUnique,
            varArity=len(funcParams),
        )

    def __eq__(self, other) -> bool:
        if isinstance(other, Func):
            return self.funcUnique == other.funcUnique
        else:
            return False

    def __hash__(self):
        return hash(self.funcUnique)


class Expr:
    def __str__(self) -> str:
        from .SimplePrinter import simplePrintExpr
        from .HaskellPrinter import haskellPrintExpr

        return simplePrintExpr(self)

    def __repr__(self) -> str:
        return self.__str__()


class Var(Expr):
    def __init__(
        self,
        varName: str,
        varType: str,
        varModule: Optional[str],
        varUnique: str,
        varArity: int,
    ) -> None:
        self.varName = varName
        self.varType = varType
        self.varModule = varModule
        self.varUnique = varUnique
        self.varArity = varArity

    def __eq__(self, other) -> bool:
        if isinstance(other, Var):
            return self.varUnique == other.varUnique
        else:
            return False

    def __hash__(self):
        return hash(self.varUnique)


class Lit(Expr):
    def __init__(self, litValue: str, litType: str) -> None:
        self.litValue = litValue
        self.litType = litType

    def __str__(self) -> str:
        return super().__str__()

    def __eq__(self, other) -> bool:
        if isinstance(other, Lit):
            return self.litValue == other.litValue and self.litType == other.litType
        else:
            return False

    def __hash__(self):
        return hash(self.litValue)


class App(Expr):
    def __init__(self, appExpr: Expr, appArg: Expr) -> None:
        self.appExpr = appExpr
        self.appArg = appArg

    def __eq__(self, other) -> bool:
        if isinstance(other, App):
            return self.appExpr == other.appExpr and self.appArg == other.appArg
        else:
            return False

    def __hash__(self):
        return hash((self.appExpr, self.appArg))


class Case(Expr):
    def __init__(self, caseExpr: Expr, caseAlts: List[Alt]) -> None:
        self.caseExpr = caseExpr
        self.caseAlts = caseAlts
        self.caseAltCount = len(caseAlts)

    def __eq__(self, other) -> bool:
        if isinstance(other, Case):
            return self.caseExpr == other.caseExpr and self.caseAlts == other.caseAlts
        else:
            return False

    def __hash__(self):
        return hash((self.caseExpr, self.caseAlts[-1]))


class Alt:
    def __init__(
        self,
        altConName: str,
        altConModule: Optional[str],
        altConVars: List[Var],
        altExpr: Expr,
    ) -> None:
        self.altConName = altConName
        self.altConModule = altConModule
        self.altConVars = altConVars
        self.altConVarCount = len(altConVars)
        self.altExpr = altExpr

    def __eq__(self, other) -> bool:
        if isinstance(other, Alt):
            return (
                self.altConName == other.altConName
                and self.altConVars == other.altConVars
                and self.altExpr == other.altExpr
            )
        else:
            return False

    def __hash__(self):
        return hash(
            (self.altConName, self.altConVars[0], self.altConVars[-1], self.altExpr)
        )
