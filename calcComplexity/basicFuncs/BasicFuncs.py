import math
from typing import Any, Callable, Dict, List

from .SympyLog import sympyLog


class BasicFunc:
    def __init__(
        self, name: str, arity: int, lambda_: Callable, sympyLambda: Any = None
    ) -> None:
        self.name = name
        self.arity = arity
        self.lambda_ = lambda_
        if sympyLambda == None:
            self.sympyLambda = lambda_
        else:
            self.sympyLambda = sympyLambda

    def __eq__(self, other) -> bool:
        if not isinstance(other, BasicFunc):
            return False
        return self.name == other.name

    def __hash__(self) -> int:
        return hash(self.name)


def log(x: int) -> int:
    if x == 0:
        return 0
    elif x > 0:
        return math.floor(math.log2(x))
    else:
        return math.floor(math.log2(-x))


basicFuncList: List[BasicFunc] = [
    BasicFunc("x", 1, lambda x: x),
    BasicFunc("x2", 1, lambda x: x**2),
    BasicFunc("x3", 1, lambda x: x**3),
    BasicFunc("logx", 1, lambda x: log(x), lambda x: sympyLog(x)),
    BasicFunc("log2x", 1, lambda x: log(x) ** 2, lambda x: sympyLog(x) ** 2),
    BasicFunc("xlogx", 1, lambda x: x * log(x), lambda x: x * sympyLog(x)),
    BasicFunc(
        "xlog2x", 1, lambda x: x * (log(x) ** 2), lambda x: x * (sympyLog(x) ** 2)
    ),
    BasicFunc(
        "x2log2x",
        1,
        lambda x: (x**2) * (log(x) ** 2),
        lambda x: (x**2) * (sympyLog(x) ** 2),
    ),
    BasicFunc("xy", 2, lambda x, y: x * y),
    BasicFunc("x_y", 2, lambda x, y: x // y, lambda x, y: x / y),
    BasicFunc("x2y", 2, lambda x, y: x**2 * y),
    BasicFunc("xy2", 2, lambda x, y: x * y**2),
    BasicFunc("x2y2", 2, lambda x, y: x**2 * y**2),
    BasicFunc("xlogy", 2, lambda x, y: x * log(y), lambda x, y: x * sympyLog(y)),
    BasicFunc("ylogx", 2, lambda x, y: y * log(x), lambda x, y: y * sympyLog(x)),
    BasicFunc("xyz", 3, lambda x, y, z: x * y * z),
]

unaryBasicFuncs = list(filter(lambda f: f.arity == 1, basicFuncList))
binaryBasicFuncs = list(filter(lambda f: f.arity == 2, basicFuncList))
ternaryBasicFuncs = list(filter(lambda f: f.arity == 3, basicFuncList))
basicFuncDict = {f.name: f for f in basicFuncList}
