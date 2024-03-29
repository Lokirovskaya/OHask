# Pretty printer that generates human-friendly result

from .Struct import *


def simplePrintExpr(expr: Expr) -> str:
    return printExpr(expr)


def printExpr(expr: Expr) -> str:
    if isinstance(expr, Var):
        return printVar(expr)
    elif isinstance(expr, Lit):
        return printLit(expr)
    elif isinstance(expr, App):
        return printApp(expr)
    elif isinstance(expr, Case):
        return printCase(expr)
    else:
        assert False


def printVar(var: Var) -> str:
    return var.varUnique


def printLit(lit: Lit) -> str:
    return lit.litValue


def printApp(app: App) -> str:
    return tryAddParen(app.appExpr) + " " + tryAddParen(app.appArg)


def printCase(case_: Case) -> str:
    return f"case {tryAddParen(case_.caseExpr)} of {{{printAlts(case_.caseAlts)}}}"


def printAlts(alts: List[Alt]) -> str:
    return "; ".join([printAlt(alt) for alt in alts])


def printAlt(alt: Alt) -> str:
    if alt.altConVarCount == 0:
        return f"{alt.altConName} -> {tryAddParen(alt.altExpr)}"
    else:
        varNameList = map(printVar, alt.altConVars)
        s = " ".join(varNameList)
        return f"{alt.altConName} {s} -> {tryAddParen(alt.altExpr)}"


def tryAddParen(expr: Expr) -> str:
    if isinstance(expr, Var) or isinstance(expr, Lit):
        return printExpr(expr)
    else:
        return f"({printExpr(expr)})"
