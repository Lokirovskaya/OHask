from typing import List

from sympy import Function, Symbol

from calcComplexity.Log import logln
from calcComplexity.constraint import SympyConstraint

from .util.ZEncoder import zDecode


constFuncs = {"+", "-", "*", "div", ":", "$", "I#", "C#", "(,)", ">", "<", ">=", "<="}


def reduceBuiltinFunctions(constrList: List[SympyConstraint]):
    for constr in constrList:
        reduceForConstr(constr)

    logln("[Simplified SymPy Constraints]")
    for constr in constrList:
        logln(str(constr))
    logln()


def reduceForConstr(constr: SympyConstraint):
    funcs = constr.rhs.atoms(Function)  # Funcs, each with args

    replaceDict = {}

    for func in funcs:
        realName = getComplRealName(func.name)
        if realName in constFuncs:
            replaceDict[func] = Symbol("C")

    if len(replaceDict) > 0:
        constr.rhs = constr.rhs.xreplace(replaceDict).doit(deep=True)


# T_RealName'unique
def getComplRealName(s: str) -> str | None:
    if not s.startswith("T_"):
        return None
    dec = zDecode(s)
    last = dec.rfind("'")
    real = dec[2:last]
    return real
