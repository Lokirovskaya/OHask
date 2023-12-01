from sympy import Symbol, Function
from sympy.core.function import UndefinedFunction
from .ZEncode import zEncode


def makeComplSymbol(funcName: str) -> Function:
    return Function("T_" + zEncode(funcName))


def makeVarSymbol(varName: str, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function("f_" + zEncode(varName))
    else:
        return Symbol("v_" + zEncode(varName))


def makeParamSymbol(funcName: str, idx: int, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function(f"p{idx}_f_{zEncode(funcName)}")
    else:
        return Symbol(f"p{idx}_v_{zEncode(funcName)}")


def makeExternalParamSymbol(funcName: str, idx: int) -> Symbol:
    return Symbol(f"p{idx}_ext_{zEncode(funcName)}")


idx = 0


def makeLambdaParamSymbol() -> Symbol:
    global idx
    s = Symbol("lp" + str(idx))
    idx += 1
    return s


def makeExternalSymbol() -> Symbol:
    return Symbol("external")


def makeLitSymbol(litVal, litType):
    return Symbol("lit")  # todo


def isComplFunc(s):
    return isinstance(s, (UndefinedFunction, Function)) and s.name.startswith("T_")


def isParam(s):
    return isinstance(s, (Symbol, UndefinedFunction, Function)) and s.name.startswith(
        "p"
    )


def isVar(s):
    return isinstance(s, Symbol) and s.name.startswith("v_")


def isLit(s):
    return isinstance(s, Symbol) and s.name == "lit"
