import sympy
from typing import List, Dict, Any
from .Api import Func, Param, Expr, Var, Lit, App, Case, ExternFunc
from .Util import zEncode

# module-defined function
modFuncTable: Dict[str, Func] = {}
# extern function
extFuncTable: Dict[str, ExternFunc] = {}
# Function full-application - Complexity map
# i.e. no partial-application, no lambda-wrapped result
funcComplMap: Dict[str, sympy.Symbol] = {}


# Complexity means computational step.
# Complexity of function-application `f(a1,...,an)` is an n-ary function `O(a1,...,an)`
# Complexity of function-literal `f` is an lambda function `λa1. ... λan. O(a1,...,an)`


def calcCompl(funcList: List[Func]):
    # Init function table
    for func in funcList:
        modFuncTable[func.funcName()] = func

    for func in funcList:
        compl = calcFuncCompl(func)
        print(f"O({func.funcName()}) = {compl}")


def calcFuncCompl(func: Func):
    return calcExprCompl(func.funcExpr())


def calcExprCompl(expr: Expr):
    if var := expr.matchVar():
        compl = calcVarCompl(var)
        print(f"var={var.varName()}, compl={compl}")
    elif lit := expr.matchLit():
        compl = calcLitCompl(lit)
    elif app := expr.matchApp():
        compl = calcAppCompl(app)
    elif case_ := expr.matchCase():
        compl = calcCaseCompl(case_)
    else:
        assert False
    return compl


def calcVarCompl(var: Var):
    name = var.varName()
    # Var is a type argument, ignore.
    if name != "$" and name.startswith("$"):
        return 0
    else:
        paramCount = var.varParamCount()
        # Var is a function with params.
        # Complexity of function-literal `f` is an lambda function `λa1. ... λan. O(a1,...,an)`
        if paramCount > 0:
            encName = zEncode(name)
            if name in funcComplMap:
                funcCompl = funcComplMap[name]
            else:
                funcCompl = sympy.symbols("O_" + encName)

            lamParams = []
            for idx in range(1, paramCount + 1):
                p = sympy.symbols(encName + "_arg" + str(idx))
                lamParams.append(p)

            # result = λ.λ.λ... (funcCompl)
            assert len(lamParams) > 0
            result = currying(lamParams, funcCompl)
            return result

        # Trivial var
        else:
            return 0


def currying(lamParams: List[sympy.Symbol], lamExpr: sympy.Symbol) -> sympy.Lambda:
    if len(lamParams) == 0:
        return lamExpr
    elif len(lamParams) == 1:
        return sympy.Lambda(lamParams[0], lamExpr)
    else:
        result = sympy.Lambda(lamParams[-1], lamExpr)
        for p in reversed(lamParams[:-1]):
            result = sympy.Lambda(p, result)
        return result


def calcLitCompl(lit: Lit):
    return 0


def calcAppCompl(app: App):
    appExpr = app.appExpr()
    appArg = app.appArg()
    appExprCompl = calcExprCompl(appExpr)
    appArgCompl = calcExprCompl(appArg)
    assert isinstance(appExprCompl, sympy.Lambda), str(appExprCompl) + " is not a lambda."
    return appExprCompl(appArgCompl)


def calcCaseCompl(case_: Case):
    caseExpr = case_.caseExpr()
    caseAlt = case_.caseAlts()[0]
    caseExprCompl = calcExprCompl(caseExpr)
    caseAltCompl = calcExprCompl(caseAlt)
    return caseExprCompl + caseAltCompl
