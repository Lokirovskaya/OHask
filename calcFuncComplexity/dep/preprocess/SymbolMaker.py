from sympy import Symbol, Function, Wild, WildFunction
from typing import List
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
