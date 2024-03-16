from __future__ import annotations
from dataclasses import dataclass
from typing import List


class Expr:
    def isValueVar(self) -> bool:
        return isinstance(self, Var) and self.isValue

    def appOne(self, arg: Expr) -> Expr:
        if self.isValueVar():
            return self  # Application on value has no effect

        # (value + value + func) x ==> value + value + func x
        elif isinstance(self, Sum):
            # If only ONE of the args is not a value
            funcIdx = -1
            for i, func in enumerate(self.args):
                if not func.isValueVar():
                    if funcIdx == -1:
                        funcIdx = i
                    else:
                        # More than one no-value args found
                        return App(self, arg)
            if funcIdx != -1:
                newArgs = self.args[:]
                newArgs[funcIdx] = newArgs[funcIdx].appOne(arg)
                return Sum(newArgs)
            else:
                return App(self, arg)

        else:
            return App(self, arg)

    def app(self, *args) -> Expr:
        ans = self
        for arg in args:
            ans = ans.appOne(arg)
        return ans

    def __call__(self, *args, **kwargs) -> Expr:
        return self.app(*args)

    def __eq__(self, other) -> bool:
        if isinstance(other, Expr):
            return self == other
        else:
            return False

    def __hash__(self):
        return hash(self)

    def __repr__(self) -> str:
        return str(self)


# A simple variable
# If var x is a value, (λz. x)(y) == x
# Otherwise, application is left unevaluated: (λz. x)(y)
@dataclass(repr=False, frozen=True)
class Var(Expr):
    name: str
    isValue: bool = True

    def __str__(self) -> str:
        if self.isValue:
            return self.name
        else:
            return "@" + self.name


# Abstraction, λx. expr
@dataclass(repr=False)
class Abstr(Expr):
    var: Var
    expr: Expr

    def __str__(self) -> str:
        return rf"\{self.var}. {tryAddParen(self.expr)}"


# Application, x y
@dataclass(repr=False)
class App(Expr):
    expr: Expr
    arg: Expr

    def __str__(self) -> str:
        return tryAddParen(self.expr) + " " + tryAddParen(self.arg)


### Special Exprs ###


class Sum(Expr):
    def __init__(self, args: List[Expr]) -> None:
        # Simplify nested sum
        self.args = []
        for arg in args:
            if isinstance(arg, Sum):
                self.args.extend(arg.args)
            else:
                self.args.append(arg)

    def __str__(self) -> str:
        argStrs = map(tryAddParen, self.args)
        s = " + ".join(argStrs)
        return s


class MaxN(Expr):
    def __init__(self, args: List[Expr]) -> None:
        self.args = args

    def __str__(self) -> str:
        argStrs = map(tryAddParen, self.args)
        s = " ".join(argStrs)
        return "maxN " + s


def tryAddParen(expr: Expr) -> str:
    if isinstance(expr, Var):
        return str(expr)
    else:
        return f"({expr})"
