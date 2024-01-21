# See OHask/funcInfoStat/gen/StatInfo.hs

from __future__ import annotations
from typing import List, Optional


class Func:
    def __init__(
        self,
        funcName: str,
        funcType: str,
        funcUnique: str,
        funcParams: List[Var],
        funcExpr: Expr,
    ) -> None:
        self.funcName = funcName
        self.funcType = funcType
        self.funcUnique = funcUnique
        self.funcParams = funcParams
        self.funcParamCount = len(funcParams)
        self.funcExpr = funcExpr
        # to be filled in genDependencies/BuildStruct.py
        self.funcParent: Optional[Func] = None


class Expr:
    pass


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


class Lit(Expr):
    def __init__(self, litValue: str, litType: str) -> None:
        self.litValue = litValue
        self.litType = litType


class App(Expr):
    def __init__(self, appExpr: Expr, appArg: Expr) -> None:
        self.appExpr = appExpr
        self.appArg = appArg


class Case(Expr):
    def __init__(self, caseExpr: Expr, caseAlts: List[Alt]) -> None:
        self.caseExpr = caseExpr
        self.caseAlts = caseAlts
        self.caseAltCount = len(caseAlts)


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
        self.altExpr = altExpr
