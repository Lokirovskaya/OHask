from sympy import Symbol, Function
from sympy.core.function import UndefinedFunction
from .ZEncode import zEncode
from ...struct import Literal


def makeComplSymbol(funcName: str) -> Function:
    return Function("T_" + zEncode(funcName))


def makeVarSymbol(varName: str, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function("f_" + zEncode(varName))
    else:
        return Symbol("v_" + zEncode(varName))


def makeParamSymbol(funcName: str, idx: int, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function(f"p{idx}f_{zEncode(funcName)}")
    else:
        return Symbol(f"p{idx}v_{zEncode(funcName)}")


def makeExternalParamSymbol(funcName: str, idx: int) -> Symbol:
    return Symbol(f"p{idx}ext_{zEncode(funcName)}")


idx = 0


def makeLambdaParamSymbol() -> Symbol:
    global idx
    s = Symbol("lp" + str(idx))
    idx += 1
    return s


def makeExternalSymbol() -> Symbol:
    return Symbol("external")


def makeLitSymbol(litVal, litType) -> Literal:
    return Literal(name=zEncode("lit_" + str(litVal)), litValue=litVal, litType=litType)


def isComplFunc(s) -> bool:
    return isinstance(s, (UndefinedFunction, Function)) and s.name.startswith("T_")


def isParam(s) -> bool:
    return isinstance(s, (Symbol, UndefinedFunction, Function)) and s.name.startswith(
        "p"
    )


def isVar(s) -> bool:
    return isinstance(s, Symbol) and s.name.startswith("v_")


def isLit(s) -> bool:
    return isinstance(s, Literal)
