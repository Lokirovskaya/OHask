from typing import Tuple
from sympy import Symbol, Function
from sympy.core.function import UndefinedFunction
from .ZEncode import zEncode, zDecode
from calcFuncComplexity.struct import Literal
import re

############## Makers ##############


def makeCompl(funcName: str) -> Function:
    return Function("T_" + zEncode(funcName))


def makeVar(varName: str, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function("f_" + zEncode(varName))
    else:
        return Symbol("v_" + zEncode(varName))


def makeParam(funcName: str, idx: int, isFunc: bool = False) -> Symbol | Function:
    if isFunc:
        return Function(f"p{idx}f_{zEncode(funcName)}")
    else:
        return Symbol(f"p{idx}v_{zEncode(funcName)}")


def makeExternalParam(funcName: str, idx: int) -> Symbol:
    return Symbol(f"p{idx}ext_{zEncode(funcName)}")


idx = 0


def makeLambdaParam() -> Symbol:
    global idx
    s = Symbol("lp" + str(idx))
    idx += 1
    return s


def makeExternalPlaceholder() -> Symbol:
    return Symbol("external")


def makeLit(litVal, litType) -> Literal:
    return Literal(name=zEncode("lit_" + str(litVal)), litValue=litVal, litType=litType)


############## Checkers ##############


def isComplFunc(s) -> bool:
    return _isFunc(s) and s.name.startswith("T_")


def isParam(s) -> bool:
    return (isinstance(s, Symbol) or _isFunc(s)) and s.name.startswith("p")


def isVar(s) -> bool:
    return isinstance(s, Symbol) and s.name.startswith("v_")


def isLit(s) -> bool:
    return isinstance(s, Literal)


############## Decoders ##############


# return: funcName
def decodeComplFunc(s) -> str:
    assert _isFunc(s) and s.name.startswith("T_"), s
    return decodeTail(s)


# return varName, isFunc
def decodeVar(s) -> Tuple[str, bool]:
    if s.name.startswith("f"):
        assert _isFunc(s)
        return decodeTail(s), True
    elif s.name.startswith("v"):
        assert isinstance(s, Symbol)
        return decodeTail(s), False
    else:
        assert False, s


# return funcName, idx, isFunc
def decodeParam(s):
    m = re.match(r"p([0-9]+)([fv])_(.+)", s.name)
    assert m, s
    idx = m.group(1)
    isFunc = m.group(2) == "f"
    tail = m.group(3)
    return zDecode(tail), idx, isFunc


############## Util ##############
def _isFunc(s):
    return isinstance(s, (UndefinedFunction, Function))


def decodeTail(s):
    return zDecode(s.name.split("_")[-1])
