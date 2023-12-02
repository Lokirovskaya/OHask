from sympy import Symbol

# Literal symbol, which carries literal value and type info


class Literal(Symbol):
    def __new__(cls, name, litValue, litType):
        obj = Symbol.__new__(cls, name)
        obj.litValue = litValue
        obj.litType = litType
        return obj
