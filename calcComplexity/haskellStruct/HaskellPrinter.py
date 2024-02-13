# Generates valid haskell expr str

from .Struct import *
from .Util import isBuiltinVar, isTupleConstructorName


def haskellPrintExpr(expr: Expr) -> str:
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
    if isBuiltinVar(var):
        return var.varName
    elif var.varModule == "Main":  # todo: specify module name
        return "MainTmp." + var.varName
    elif var.varModule != None:
        return var.varModule + "." + var.varName
    else:
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
    ans = ""

    if isTupleConstructorName(alt.altConName):
        ans += alt.altConName
    else:
        ans += "(" + alt.altConName + ")"

    if alt.altConVarCount > 0:
        varNameList = map(printVar, alt.altConVars)
        s = " ".join(varNameList)
        ans += " " + s

    ans += f" -> {tryAddParen(alt.altExpr)}"

    return ans


def tryAddParen(expr: Expr) -> str:
    if isinstance(expr, Lit):
        return printExpr(expr)
    else:
        return f"({printExpr(expr)})"
