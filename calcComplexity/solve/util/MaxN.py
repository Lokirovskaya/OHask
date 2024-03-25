from sympy import Symbol, Function


# MaxN(C0, C1, ..., T0, T1, ...) = MaxN(T0, T1, ...)
# MaxN(T0, T1, ...) = MaxN(T0, T1, ...)
# MaxN(C0, C1, ...) = C
# MaxN(X) = X

const = Symbol("C")


class MaxN(Function):
    name = "MaxN"

    @classmethod
    def eval(cls, *args):  # type: ignore
        if len(args) == 0:
            return 0
        elif len(args) == 1:
            return args[0]
        else:
            return None

    def doit(self, deep=False, **hints):  # type: ignore
        # len(args) here should > 1, otherwise, it will be previously reduced by `eval`
        args = list(self.args)

        if deep:
            for i in range(len(args)):
                args[i] = args[i].doit(deep=deep, **hints)  # type: ignore

        def isConst(expr):
            if expr == const:
                return True
            atoms = expr.atoms(Symbol, Function)
            return len(atoms) == 1 and const in atoms 

        args = list(filter(lambda a: not isConst(a), args))
        
        if len(args) == 0:
            return const
        else:
            return MaxN(*args)
