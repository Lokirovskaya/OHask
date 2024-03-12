from __future__ import annotations
from dataclasses import dataclass
from typing import List


class Expr:
    def appOne(self, arg: Expr) -> Expr:
        if isinstance(self, Var) and self.isValue:
            return self  # Application on value has no effect

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


@dataclass(repr=False)
class Add(Expr):
    left: Expr
    right: Expr

    def __str__(self) -> str:
        return tryAddParen(self.left) + " + " + tryAddParen(self.right)


@dataclass(repr=False)
class MaxN(Expr):
    args: List[Expr]

    def __str__(self) -> str:
        argStrs = map(tryAddParen, self.args)
        s = " ".join(argStrs)
        return "maxN " + s


def tryAddParen(expr: Expr) -> str:
    if isinstance(expr, Var):
        return str(expr)
    else:
        return f"({expr})"
