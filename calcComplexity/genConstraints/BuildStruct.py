from typing import Any, Dict, List

from calcComplexity.haskellStruct import Alt, App, Case, Expr, Func, Lit, Var


def buildStruct(funcsData: List[Any]) -> List[Func]:
    funcList = []
    unique2FuncDict: Dict[str, Func] = {}

    for funcData in funcsData:
        unique = funcData["funcUnique"]
        func = buildFunc(funcData)
        funcList.append(func)
        unique2FuncDict[unique] = func

    # Fill field funcParent
    for unique, func in unique2FuncDict.items():
        parentUnique = func.funcParentUnique
        if parentUnique != None:
            func.funcParent = unique2FuncDict[parentUnique]

    return funcList


def buildFunc(funcData) -> Func:
    return Func(
        funcName=funcData["funcName"],
        funcType=funcData["funcType"],
        funcUnique=funcData["funcUnique"],
        funcParentUnique=funcData["funcParentUnique"],
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
