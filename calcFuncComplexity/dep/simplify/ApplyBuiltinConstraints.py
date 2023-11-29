from typing import List
from ..struct.Constraint import Constraint
from ..preprocess.SymbolMaker import makeExternalSymbol
from ..preprocess.ZEncode import zDecode


extSym = makeExternalSymbol()


def applyBuiltinConstraints(constrList: List[Constraint]):
    result = []
    
    for constr in constrList:
        if constr.rhs.expr == extSym:
            realName = getRealName(constr.lhs.name)
            if compl := findBuiltin(realName):
                constr.substitute(extSym, compl)
            # Not a built-in function, remove from constraints
            else:
                continue
        result.append(constr)
    
    constrList[:] = result
            


def getRealName(s):
    assert s.startswith("T_")
    encName = s[2:]
    decName = zDecode(encName)
    realName = decName.split(".")[0]
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
