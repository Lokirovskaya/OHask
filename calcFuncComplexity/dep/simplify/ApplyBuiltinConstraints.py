from typing import List
from ..struct.Constraint import Constraint
from ..preprocess.SymbolMaker import makeExternalSymbol


extSym = makeExternalSymbol()


def applyBuiltinConstraints(constrList: List[Constraint]):
    for constr in constrList:
        constr.substitute(extSym, 1)
