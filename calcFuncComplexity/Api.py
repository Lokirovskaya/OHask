from __future__ import annotations
from typing import List, Dict, Any, Optional


class Func:
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def funcName(self) -> str:
        return self.data["funcName"]

    def funcType(self) -> str:
        return self.data["funcType"]

    def funcParams(self) -> List[Param]:
        return list(map(Param, self.data["funcParams"]))

    def funcExpr(self) -> Expr:
        return Expr(self.data["funcExpr"])


class Param:
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def paramName(self) -> str:
        return self.data["paramName"]

    def paramType(self) -> str:
        return self.data["paramType"]


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
    def varName(self) -> str:
        return self.data["varName"]

    def varType(self) -> str:
        return self.data["varType"]


class Lit(Expr):
    def litValue(self) -> str:
        return self.data["litValue"]

    def litType(self) -> str:
        return self.data["litType"]


class App(Expr):
    def appExpr(self) -> Expr:
        return Expr(self.data["appExpr"])

    def appArg(self) -> Expr:
        return Expr(self.data["appArg"])


class Case(Expr):
    def caseExpr(self) -> Expr:
        return Expr(self.data["caseExpr"])

    def caseAlts(self) -> Expr:
        return Expr(self.data["caseAlts"])
