from sympy import Symbol


def const(*_):
    return Symbol("C")


constFuncs = {"+", "-", "*", "div", ":", "$", "I#", "C#", "(,)"}
