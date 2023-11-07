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

    def funcParamCount(self) -> int:
        return len(self.data["funcParams"])

    def funcExpr(self) -> Expr:
        return Expr._makeExpr(self.data["funcExpr"])


class Param:
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def paramName(self) -> str:
        return self.data["paramName"]

    def paramType(self) -> str:
        return self.data["paramType"]


class Expr:
    def _makeExpr(data: Dict[str, Any]) -> Expr:
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
        self.data = data

    def varName(self) -> str:
        return self.data["varName"]

    def varType(self) -> str:
        return self.data["varType"]

    def varParams(self) -> [str]:
        return self.data["varParams"]
    
    def varParamCount(self) -> int:
        return len(self.data["varParams"])


class Lit(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def litValue(self) -> str:
        return self.data["litValue"]

    def litType(self) -> str:
        return self.data["litType"]


class App(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def appExpr(self) -> Expr:
        return Expr._makeExpr(self.data["appExpr"])

    def appArg(self) -> Expr:
        return Expr._makeExpr(self.data["appArg"])


class Case(Expr):
    def __init__(self, data: Dict[str, Any]):
        self.data = data

    def caseExpr(self) -> Expr:
        return Expr._makeExpr(self.data["caseExpr"])

    def caseAlts(self) -> Expr:
        return list(map(Expr._makeExpr, self.data["caseAlts"]))


class ExternFunc:
    def __init__(self, name: str, type_: str):
        self._name = name
        self._type = type_
        self._paramCount = self._getParamCount(type_)

    def funcName(self) -> str:
        return self._name

    def funcType(self) -> str:
        return self._type

    def funcParamCount(self) -> int:
        return self._paramCount

    def _getParamCount(self, type_):
        result = 0
        # ignore `->` in parens
        parenCount = 0
        bracketCount = 0
        braceCount = 0
        for i in range(len(type_) - 1):
            c = type_[i]
            nextC = type_[i + 1]
            if c == "-" and nextC == ">":
                if parenCount + bracketCount + braceCount == 0:  # not in any paren
                    result += 1
            elif c == "(":
                parenCount += 1
            elif c == ")":
                parenCount -= 1
            elif c == "[":
                bracketCount += 1
            elif c == "]":
                bracketCount -= 1
            elif c == "{":
                braceCount += 1
            elif c == "}":
                braceCount -= 1
        return result
