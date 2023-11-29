from sympy import Function
from sympy.core.numbers import Integer, IntegerConstant


class MaxCompl(Function):
    name = "MaxCompl"

    @classmethod
    def eval(cls, m, n):
        if isInt(m):
            return n
        if isInt(n):
            return m

    def doit(self, deep=False, **hints):
        m, n = self.args
        if deep:
            m, n = m.doit(deep=deep, **hints), n.doit(deep=deep, **hints)



def isInt(x):
    return isinstance(x, (int, Integer, IntegerConstant))
