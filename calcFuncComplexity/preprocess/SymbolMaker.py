from sympy import Symbol, Function
from typing import List
from .ZEncode import zEncode


def makeComplSymbol(funcName: str) -> Function:
    return Function("T_" + zEncode(funcName))


def makeVarSymbol(varName: str) -> Symbol:
    return Symbol("v_" + zEncode(varName))


def makeFuncSymbol(funcName: str) -> Function:
    return Function("f_" + zEncode(funcName))


def makeParamSymbol(funcName: str, idx: int) -> Symbol:
    return Symbol(f"p{idx}_{zEncode(funcName)}")


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
