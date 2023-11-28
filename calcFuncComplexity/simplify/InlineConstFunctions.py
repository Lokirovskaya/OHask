from typing import List
from ..struct.Constraint import Constraint
from ..struct.MyLambda import MyLambda
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


def isConst(x):
    return isinstance(x, MyLambda) and isinstance(
        x.expr, (int, Integer, IntegerConstant)
    )
