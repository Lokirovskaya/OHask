from sympy import Function, Lambda, Symbol


class HigherSymbol:
    def __new__(cls, name, arity=0) -> Symbol | Lambda:
        if arity == 0:
            return Symbol(name)
        elif arity == 1:
            param = _getUniqueSymbol()
            return Lambda(param, Function(name)(param))  # type: ignore
        else:
            params = [_getUniqueSymbol() for _ in range(arity)]
            core = Function(name)(*params)
            for param in reversed(params):
                core = Lambda(param, core)
            return core


_uuid = 0


def _getUniqueSymbol():
    global _uuid
    name = f"__u{_uuid}"
    sym = Symbol(name)
    _uuid += 1
    return sym
