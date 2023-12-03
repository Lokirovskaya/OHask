from typing import List
from .Target import Target
import datetime
from ...dep.preprocess.ZEncode import zDecode
from ...dep.preprocess.SymbolMaker import isLit
from sympy.utilities.iterables import iterable


def genTestFile(targetList: List[Target], filePath: str):
    with open(filePath, "w") as f:
        curDatetime = str(datetime.datetime.now())
        f.write(f"-- {curDatetime}\n")
        f.write("{-# OPTIONS_GHC -w #-}\n")
        f.write("{-# HLINT ignore #-}\n")
        f.write("\n")

        for target in targetList:
            f.write(genTargetHaskell(target) + "\n\n")


literalFuncs = ["I#", "C#", "IS", "fromInteger"]


def genTargetHaskell(target) -> str:
    result = f"-- {target}\n"
    result += f"expr{target.id} = "
    first = True

    def runExpr(expr):
        def inner(expr):
            nonlocal result
            name = getRealName(expr.name)

            if name in literalFuncs:
                lit = expr.args[0]
                assert isLit(lit), f"Expr {lit} is not a Literal"
                result += lit.litValue.rstrip("#")
                return

            # Surround the var name by `()` to satisfy infix functions
            result += "(" + name + ")"

            if iterable(expr.args):
                for arg in expr.args:
                    runExpr(arg)

        nonlocal result
        nonlocal first
        if first:
            first = False
            inner(expr)
        else:
            result += "("
            inner(expr)
            result += ")"

    runExpr(target.expr)
    return result


def getRealName(s: str) -> str:
    tail = s.split("_")[-1]
    decName = zDecode(tail)
    realName = decName.split(".")[0]
    return realName
