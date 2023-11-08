from __future__ import annotations
from typing import List, Dict, Any, Optional


class Func:
    def __init__(self, data: Dict[str, Any]):
        self.funcName: str = data["funcName"]
        self.funcType: str = data["funcType"]
        self.funcParams: List[Param] = list(map(Param, data["funcParams"]))
        self.funcParamCount: int = len(self.funcParams)
        self.funcExpr: Expr = Expr.makeExpr(data["funcExpr"])


class Param:
    def __init__(self, data: Dict[str, Any]):
        self.paramName: str = data["paramName"]
        self.paramType: str = data["paramType"]


class Expr:
    def makeExpr(data: Dict[str, Any]) -> Expr:
        if data["exprKind"] == "Var":
            return Var(data)
        elif data["exprKind"] == "Lit":
            return Lit(data)
        elif data["exprKind"] == "App":
            return App(data)
        elif data["exprKind"] == "Case":
            return Case(data)
        else:
            assert False

    def matchVar(self) -> Optional[Var]:
        if isinstance(self, Var):
            return self
        else:
            return None

    def matchLit(self) -> Optional[Lit]:
        if isinstance(self, Lit):
            return self
        else:
            return None

    def matchApp(self) -> Optional[App]:
        if isinstance(self, App):
            return self
        else:
            return None

    def matchCase(self) -> Optional[Case]:
        if isinstance(self, Case):
            return self
        else:
            return None


class Var(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.varName: str = data["varName"]
        self.varType: str = data["varType"]
        self.varParams: List[str] = data["varParams"]
        self.varParamCount: int = len(self.varParams)


class Lit(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.litValue: str = data["litValue"]
        self.litType: str = data["litType"]


class App(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.appExpr: Expr = Expr.makeExpr(data["appExpr"])
        self.appArg: Expr = Expr.makeExpr(data["appArg"])


class Case(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.caseExpr: Expr = Expr.makeExpr(data["caseExpr"])
        self.caseAlts: List[Expr] = list(map(Expr.makeExpr, data["caseAlts"]))
        self.caseAltCount: int = len(self.caseAlts)


