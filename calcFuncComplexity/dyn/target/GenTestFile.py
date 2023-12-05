from typing import List
from .Target import Target
import datetime
from calcFuncComplexity.util.symbol import isLit, isParam, decodeTail, decodeParam
from sympy.utilities.iterables import iterable
from sympy.core import postorder_traversal


def genTestFile(targetList: List[Target], filePath: str):
    with open(filePath, "w") as f:
        curDatetime = str(datetime.datetime.now())
        f.write(f"-- {curDatetime}\n")
        f.write("{-# OPTIONS_GHC -w #-}\n")
        f.write("{-# HLINT ignore #-}\n")
        f.write("\n")

        for target in targetList:
            fillInputVarList(target)
            f.write(genTargetHaskell(target) + "\n\n")


def fillInputVarList(target):
    realParams = [var for var in postorder_traversal(target.expr) if isParam(var)]
    for param in realParams:
        target.newInputVar(param)


ignoreFuncs = ["I#", "C#", "IS", "fromInteger"]


def genTargetHaskell(target) -> str:
    def runExpr(expr, noParen=False):
        def inner(expr):
            nonlocal result
            name = getRealName(expr)

            if name in ignoreFuncs:
                runExpr(expr.args[0], noParen=True)
                return

            elif isLit(expr):
                result += expr.litValue.rstrip("#")

            # Params are treated as unknown input var
            elif isParam(expr):
                inputVarIdx = target.getInputVar(expr)
                result += f"__p{inputVarIdx}"

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

    result = f"-- {target}\n"
    # __p0 __p1 ...
    inputVars = [f"__p{i}" for i in range(target.inputVarLen())]
    for i, s in enumerate(inputVars):
        result += f"-- {s} = {target.getRealVar(i)}\n"
    inputVarStr = "".join([" " + s for s in inputVars])
    result += f"__expr{target.id}{inputVarStr} = "

    runExpr(target.expr, noParen=True)
    return result


def getRealName(s) -> str:
    tail = decodeTail(s)
    realName = tail.split(".")[0]
    return realName
