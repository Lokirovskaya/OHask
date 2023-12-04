from typing import List
from .Target import Target
import datetime
from calcFuncComplexity.util.symbol import isLit, isParam, decodeTail, decodeParam
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


ignoreFuncs = ["I#", "C#", "IS", "fromInteger"]


def genTargetHaskell(target) -> str:
    result = f"-- {target}\n"
    result += f"__expr{target.id} = "

    def runExpr(expr, noParen=False):
        def inner(expr):
            nonlocal result
            name = getRealName(expr)

            if name in ignoreFuncs:
                runExpr(expr.args[0], noParen=True)
                return

            elif isLit(expr):
                result += expr.litValue.rstrip("#")

            elif isParam(expr):
                funcName, idx, isFunc = decodeParam(expr)
                funcRealName = funcName.split('.')[0]
                result += f"__p{idx}_{funcRealName}"

            else:
                # Surround the var name by `()` to satisfy infix functions
                result += "(" + name + ")"

            if iterable(expr.args):
                for arg in expr.args:
                    runExpr(arg)

        nonlocal result
        if noParen:
            inner(expr)
        else:
            result += "("
            inner(expr)
            result += ")"

    runExpr(target.expr, noParen=True)
    return result


def getRealName(s) -> str:
    tail = decodeTail(s)
    realName = tail.split(".")[0]
    return realName
