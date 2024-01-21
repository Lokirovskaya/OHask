from typing import Any, List

from calcComplexity.haskellStruct import Alt, App, Case, Expr, Func, Lit, Var


funcList: List[Func] = []

def buildStruct(funcsData: List[Any]) -> List[Func]:
    for funcData in funcsData:
        funcList.append(buildFunc(funcData))
    return funcList


def buildFunc(funcData) -> Func:
    return Func(
        funcName=funcData["funcName"],
        funcType=funcData["funcType"],
        funcUnique=funcData["funcUnique"],
        funcParams=[buildVar(paramData) for paramData in funcData["funcParams"]],
        funcExpr=buildExpr(funcData["funcExpr"]),
    )


def buildExpr(exprData) -> Expr:
    kind = exprData["exprKind"]
    if kind == "Var":
        return buildVar(exprData)
    elif kind == "Lit":
        return buildLit(exprData)
    elif kind == "App":
        return buildApp(exprData)
    elif kind == "Case":
        return buildCase(exprData)
    elif kind == "Lam":
        pass

    assert False, f"Illegal expr kind {kind}"


def buildVar(varData) -> Var:
    return Var(
        varName=varData["varName"],
        varType=varData["varType"],
        varModule=varData["varModule"],
        varUnique=varData["varUnique"],
        varArity=varData["varArity"],
    )


def buildLit(litData) -> Lit:
    return Lit(litValue=litData["litValue"], litType=litData["litType"])


def buildApp(appData) -> App:
    appExpr = buildExpr(appData["appExpr"])
    appArg = buildExpr(appData["appArg"])
    return App(appExpr, appArg)


def buildCase(caseData) -> Case:
    caseExpr = buildExpr(caseData["caseExpr"])
    caseAlts = [buildAlt(altData) for altData in caseData["caseAlts"]]
    return Case(caseExpr, caseAlts)


def buildAlt(altData) -> Alt:
    altConName = altData["altCon"]["conName"]
    altConModule = altData["altCon"]["conModule"]
    altConVars = [buildVar(varData) for varData in altData["altVars"]]
    altExpr = buildExpr(altData["altExpr"])
    return Alt(altConName, altConModule, altConVars, altExpr)
