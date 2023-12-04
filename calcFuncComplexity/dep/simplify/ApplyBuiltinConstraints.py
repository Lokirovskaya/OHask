from typing import List
from calcFuncComplexity.struct import Constraint
from calcFuncComplexity.util.symbol import makeExternalPlaceholder, decodeComplFunc


extSym = makeExternalPlaceholder()


def applyBuiltinConstraints(constrList: List[Constraint]):
    result = []

    for constr in constrList:
        if constr.rhs.expr == extSym:
            realName = getRealName(constr.lhs)
            if compl := findBuiltin(realName):
                constr.substitute(extSym, compl)
            # Not a built-in function, remove from constraints
            else:
                continue
        result.append(constr)

    constrList[:] = result


def getRealName(s):
    funcName = decodeComplFunc(s)
    realName = funcName.split(".")[0]
    return realName


builtinFuncs = {
    ":": 1,
    "I#": 1,
    "C#": 1,
    "fromInteger": 1,
}


def findBuiltin(s):
    if s in builtinFuncs:
        return builtinFuncs[s]
    else:
        return None
