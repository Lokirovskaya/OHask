from __future__ import annotations
from typing import List, Dict, Any, Optional


class Func:
    def __init__(self, data: Dict[str, Any]):
        self.funcName: str = data["funcName"]
        self.funcType: str = data["funcType"]
        self.funcParams: List[Param] = list(map(Param, data["funcParams"]))
        self.funcParamCount: int = len(self.funcParams)
        self.funcExpr: Expr = Expr.makeExpr(data["funcExpr"])

    # return -1: Given var is not a param of the function
    def findParam(self, var: Var) -> int:
        i = 0
        for param in self.funcParams:
            if var.varName == param.paramName:
                assert var.varType == param.paramType, (
                    f"VarName: {var.varName}, VarType: {var.varType}, but ParamType {param.paramType}",
                )
                return i
            i += 1
        return -1


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

    def children(self) -> List[Expr]:
        if app := self.matchApp():
            return [app.appExpr, app.appArg]
        elif case_ := self.matchCase():
            return [case_.caseExpr] + case_.caseAlts
        else:
            return []


class Var(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.varName: str = data["varName"]
        self.varType: str = data["varType"]
        self.varParams: List[str] = data["varParams"]
        self.varParamCount: int = len(self.varParams)

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Var):
            return False
        return self.varName == __value.varName and self.varType == __value.varType

    def __hash__(self) -> int:
        return hash((self.varName, self.varType))


class Lit(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.litValue: str = data["litValue"]
        self.litType: str = data["litType"]

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Lit):
            return False
        return self.litValue == __value.litValue and self.litType == __value.litType

    def __hash__(self) -> int:
        return hash((self.litValue, self.litType))


class App(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.appExpr: Expr = Expr.makeExpr(data["appExpr"])
        self.appArg: Expr = Expr.makeExpr(data["appArg"])


class Case(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.caseExpr: Expr = Expr.makeExpr(data["caseExpr"])
        self.caseAlts: List[Expr] = list(map(Expr.makeExpr, data["caseAlts"]))
        self.caseAltCount: int = len(self.caseAlts)
