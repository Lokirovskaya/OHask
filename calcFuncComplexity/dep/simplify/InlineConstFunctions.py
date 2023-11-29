from typing import List
from ...struct import Constraint, MyLambda
from sympy.core.numbers import Integer, IntegerConstant


def inlineConstFunctions(constrList: List[Constraint]):
    constConstrs = []
    for constr in constrList:
        if isConst(constr.rhs):
            constConstrs.append(constr)

    newConstConstrs = []

    while True:
        for constr in constrList:
            if isConst(constr.rhs):
                continue

            for c in constConstrs:
                constr.substitute(c.lhs, c.rhs)

            if isConst(constr.rhs):
                newConstConstrs.append(constr)

        if len(newConstConstrs) > 0:
            constConstrs = newConstConstrs
            newConstConstrs = []
        else:
            break

    # Remove all const constraints from original constr-list
    result = []
    for constr in constrList:
        if not isConst(constr.rhs):
            result.append(constr)
    constrList[:] = result


def isConst(x):
    return isinstance(x, MyLambda) and isinstance(
        x.expr, (int, Integer, IntegerConstant)
    )
