from typing import Callable, Dict
import math


def log(x: int) -> int:
    if x == 0:
        return 0
    elif x > 0:
        return math.floor(math.log2(x))
    else:
        return math.floor(math.log2(-x))


basicFuncs1: Dict[str, Callable] = {
    "x": lambda x: x,
    "x2": lambda x: x * x,
    "x3": lambda x: x * x * x,
    "logx": lambda x: log(x),
    "log2x": lambda x: log(x) * log(x),
    "xlogx": lambda x: x * log(x),
    "xlog2x": lambda x: x * log(x) * log(x),
    "x2logx": lambda x: x * x * log(x),
    "x2log2x": lambda x: x * x * log(x) * log(x),
}

basicFuncs2: Dict[str, Callable] = {
    "xy": lambda x, y: x * y,
    "x_y": lambda x, y: x // y,
    "x2y": lambda x, y: x * x * y,
    "xy2": lambda x, y: x * y * y,
    "x2y2": lambda x, y: x * x * y * y,
    "xlogy": lambda x, y: x * log(y),
    "ylogx": lambda x, y: y * log(x),
    "logxy": lambda x, y: log(x * y),
}

basicFuncs3: Dict[str, Callable] = {
    "xyz": lambda x, y, z: x * y * z,
}
