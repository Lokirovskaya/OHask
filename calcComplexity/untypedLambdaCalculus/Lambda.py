from __future__ import annotations


class Expr:
    def subst(self, old: Var, new: Expr) -> Expr:
        if isinstance(self, Var):
            if self.isValue:
                if self == old:
                    return new
                else:
                    return old
            else:
                if self == old:
                    return new
                else:
                    return UnevalSubst(self, old, new)

        elif isinstance(self, Abstr):
            # (λy. t1)[x -> s] = λy. t1[x -> s], if y != x and y ∉ FV(s)
            if self.var == old:
                assert False, f"Substitution does not work on bounded var {self.var}"
            # todo: free vars check
            return Abstr(self.var, self.expr.subst(old, new))

        elif isinstance(self, App):
            return App(self.expr.subst(old, new), self.arg.subst(old, new))

        else:
            assert isinstance(self, UnevalSubst)
            return UnevalSubst(self, old, new)

    def appOne(self, arg: Expr) -> Expr:
        if isinstance(self, Var) and self.isValue:
            return self  # Application on value has no effect

        elif isinstance(self, Abstr):
            return self.expr.subst(old=self.var, new=arg)

        else:
            return App(self, arg)

    def app(self, *args) -> Expr:
        ans = self
        for arg in args:
            ans = ans.appOne(arg)
        return ans

    def __call__(self, *args, **kwargs) -> Expr:
        return self.app(*args)


# A simple variable
# If var x is a value, (λz. x)(y) == x
# Otherwise, an unevaluated substitution remained: (λz. x)(y) == x[z -> y]
class Var(Expr):
    def __init__(self, name: str, isValue: bool = True, **kwargs) -> None:
        self.name = name
        self.isValue = isValue
        self.kwargs = kwargs

    def __str__(self) -> str:
        if self.isValue:
            return self.name
        else:
            return "@" + self.name

    def __eq__(self, other) -> bool:
        if isinstance(other, Var):
            return self.name == other.name and self.isValue == other.isValue
        else:
            return False

    def __hash__(self):
        return hash((self.name, self.isValue))


# Abstraction, λx. expr
class Abstr(Expr):
    def __init__(self, var: Var, expr: Expr) -> None:
        self.var = var
        self.expr = expr

    def __str__(self) -> str:
        return rf"\{self.var}. {tryAddParen(self.expr)}"


# Application, x y
class App(Expr):
    def __init__(self, expr: Expr, arg: Expr) -> None:
        self.expr = expr
        self.arg = arg

    def __str__(self) -> str:
        # # Trick, make result more readable
        # if (
        #     isinstance(self.expr, App)
        #     and isinstance(self.expr.expr, Var)
        #     and self.expr.expr.name == "+"
        # ):
        #     lhs = self.expr.arg
        #     rhs = self.arg
        #     return tryAddParen(lhs) + " + " + tryAddParen(rhs)
        # else:
        #     return tryAddParen(self.expr) + " " + tryAddParen(self.arg)
        return tryAddParen(self.expr) + " " + tryAddParen(self.arg)


# Unevaluated Substitution, expr[old -> new]
class UnevalSubst(Expr):
    def __init__(self, expr: Expr, old: Var, new: Expr) -> None:
        self.expr = expr
        self.old = old
        self.new = new

    def __str__(self) -> str:
        return f"{tryAddParen(self.expr)}[{self.old} -> {tryAddParen(self.new)}]"


def tryAddParen(expr: Expr) -> str:
    if isinstance(expr, Var) or isinstance(expr, UnevalSubst):
        return str(expr)
    else:
        return f"({expr})"
