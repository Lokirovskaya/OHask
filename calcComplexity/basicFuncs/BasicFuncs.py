from typing import Callable, Dict, List
import math
from dataclasses import dataclass


@dataclass(frozen=True)
class BasicFunc:
    name: str
    arity: int
    lambda_: Callable
    str_: Callable


def log(x: int) -> int:
    if x == 0:
        return 0
    elif x > 0:
        return math.floor(math.log2(x))
    else:
        return math.floor(math.log2(-x))


basicFuncList: List[BasicFunc] = [
    BasicFunc("x", 1, lambda x: x, lambda x: str(x)),
    BasicFunc("x2", 1, lambda x: x**2, lambda x: f"{x}^2"),
    BasicFunc("x3", 1, lambda x: x**3, lambda x: f"{x}^3"),
    BasicFunc("logx", 1, lambda x: log(x), lambda x: f"log({x})"),
    BasicFunc("log2x", 1, lambda x: log(x) ** 2, lambda x: f"log({x})^2"),
    BasicFunc("xlogx", 1, lambda x: x * log(x), lambda x: f"{x} * log({x})"),
    BasicFunc("xlog2x", 1, lambda x: x * (log(x) ** 2), lambda x: f"{x} * log({x})^2"),
    BasicFunc(
        "x2log2x", 1, lambda x: (x**2) * (log(x) ** 2), lambda x: f"{x}^2 * log({x})^2"
    ),
    BasicFunc("xy", 2, lambda x, y: x * y, lambda x, y: f"{x} * {y}"),
    BasicFunc("x_y", 2, lambda x, y: x // y, lambda x, y: f"{x} / {y}"),
    BasicFunc("x2y", 2, lambda x, y: x**2 * y, lambda x, y: f"{x}^2 * {y}"),
    BasicFunc("xy2", 2, lambda x, y: x * y**2, lambda x, y: f"{x} * {y}^2"),
    BasicFunc("x2y2", 2, lambda x, y: x**2 * y**2, lambda x, y: f"{x}^2 * {y}^2"),
    BasicFunc("xlogy", 2, lambda x, y: x * log(y), lambda x, y: f"{x} * log({y})"),
    BasicFunc("ylogx", 2, lambda x, y: y * log(x), lambda x, y: f"log({x}) * {y}"),
    BasicFunc("xyz", 3, lambda x, y, z: x * y * z, lambda x, y, z: f"{x} * {y} * {z}"),
]

unaryBasicFuncs = list(filter(lambda f: f.arity == 1, basicFuncList))
binaryBasicFuncs = list(filter(lambda f: f.arity == 2, basicFuncList))
ternaryBasicFuncs = list(filter(lambda f: f.arity == 3, basicFuncList))
basicFuncDict = {f.name: f for f in basicFuncList}
