from sympy import Symbol, Function
from typing import List
from .ZEncode import zEncode

# Symbol naming convention:
# O_f: Complexity of function f
# s_v: Scale of var v
# p+idx_f: Scale of idx-th param of function f. idx starts from 0
# lp+uuid: Lambda param, chosen fresh, i.e. with uuid


def makeComplSymbol(funcName: str, params: List[Symbol]) -> Function:
    return Function("T_" + zEncode(funcName))(*params)


def makeVarSymbol(varName: str) -> Symbol:
    return Symbol("v_" + zEncode(varName))


def makeParamSymbol(funcName: str, idx: int) -> Symbol:
    return Symbol(f"p{idx}_{zEncode(funcName)}")


idx = 0


def makeLambdaParamSymbol() -> Symbol:
    global idx
    s = Symbol("lp" + str(idx))
    idx += 1
    return s
