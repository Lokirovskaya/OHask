from sympy import Lambda, Symbol

from .HigherOrder import HigherSymbol
from .ZEncoder import zEncode


def makeSymbol(name: str, arity: int = 0) -> Symbol | Lambda:
    return HigherSymbol(zEncode(name), arity)


def makeScaleSymbol(scaleName: str) -> Lambda:
    lam = HigherSymbol(zEncode("S_" + scaleName), arity=1)
    assert isinstance(lam, Lambda)
    return lam

def makeUnknownSymbol() -> Symbol:
    return Symbol("?")