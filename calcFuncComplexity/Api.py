from __future__ import annotations
from typing import List, Dict, Any, Optional

# Structure of stat info, for detail, see ./funcAppStat/gen/StatInfo.hs


class Func:
    def __init__(
        self,
        data: Dict[str, Any],
        funcName: str = None,
        funcDisplayName: str = None,
        funcType: str = None,
        funcParams: List[Param] = None,
        funcExpr: Expr = None,
    ):
        if funcName != None:
            self.funcName: str = funcName
        else:
            self.funcName: str = data["funcUnique"]

        if funcDisplayName != None:
            self.funcDisplayName: str = funcDisplayName
        else:
            self.funcDisplayName: str = data["funcName"]

        if funcType != None:
            self.funcType: str = funcType
        else:
            self.funcType: str = data["funcType"]

        if funcParams != None:
            self.funcParams: List[Param] = funcParams
        else:
            self.funcParams: List[Param] = list(map(Param, data["funcParams"]))

        self.funcParamCount: int = len(self.funcParams)

        if funcExpr != None:
            self.funcExpr: Expr = funcExpr
        else:
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
        self.paramName: str = data["paramUnique"]
        self.paramDisplayName: str = data["paramName"]
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
        elif data["exprKind"] == "Lam":
            return Lam(data)
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

    def matchLam(self) -> Optional[Lam]:
        if isinstance(self, Lam):
            return self
        else:
            return None

    def children(self) -> List[Expr]:
        if app := self.matchApp():
            return [app.appExpr, app.appArg]
        elif case_ := self.matchCase():
            return [case_.caseExpr] + case_.caseAlts
        elif lam := self.matchLam():
            return [lam.lamExpr]
        else:
            return []


class Var(Expr):
    def __init__(
        self,
        data: Dict[str, Any],
        varName: str = None,
        varDisplayName: str = None,
        varType: str = None,
        varParamTypes: str = None,
    ):
        if varName != None:
            self.varName: str = varName
        else:
            self.varName: str = data["varUnique"]

        if varDisplayName != None:
            self.varDisplayName: str = varDisplayName
        else:
            self.varDisplayName: str = data["varName"]

        if varType != None:
            self.varType: str = varType
        else:
            self.varType: str = data["varType"]

        if varParamTypes != None:
            self.varParamTypes: List[str] = varParamTypes
        else:
            self.varParamTypes: List[str] = data["varParams"]

        self.varParamCount: int = len(self.varParamTypes)

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


class Lam(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.lamExpr: Expr = Expr.makeExpr(data["lamExpr"])
        self.lamParams: List[Param] = list(map(Param, data["lamParams"]))
        self.lamParamCount: int = len(self.lamParams)
