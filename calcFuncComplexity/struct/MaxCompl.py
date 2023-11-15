from sympy import Function


class MaxCompl(Function):
    name = "MaxCompl"

    @classmethod
    def eval(cls, m, n):
        if m == 0:
            return n
        if n == 0:
            return m

    def doit(self, deep=False, **hints):
        m, n = self.args
        if deep:
            m, n = m.doit(deep=deep, **hints), n.doit(deep=deep, **hints)

        pass
