from typing import List

from calcComplexity.runDynExec import Group
import calcComplexity.genConstraints.VarDef as varDef
import calcComplexity.haskellStruct as haskell


def genHaskellProgram(groupList: List[Group]):
    text = ""
    for i, group in enumerate(groupList):
        text += showGroup(group, f"g{i}") + "\n"

    with open("./run/DynExprs.hs", "w") as f:
        f.write(text)


def showGroup(group: Group, groupName: str) -> str:
    ans = ""
    ident = "  "

    # group decl
    varsStr = " ".join([hask(var) for var in group.paramVars])
    ans += f"{groupName} {varsStr} = \n"

    domVars = group.domVars
    paramVars = group.paramVars

    ans += ident + "let\n"

    # letVars = domVars - paramVars
    # Every letVar should have corresponding varDef
    letVars = domVars.difference(paramVars)
    if len(letVars) > 0:

        for var in letVars:
            defStr = varDef.getDef(var)
            # `maybe` vars
            ans += ident * 2 + f"mv'{hask(var)} = {defStr}\n"

    # exprs to be dynamically executed
    for sym in group.exprSymList:
        # exprDepVars = (expr.vars /\ group.domVars) - group.paramVars

        info = sym.exprInfo
        exprDepVars = info.varSet.intersection(domVars).difference(paramVars)
        symName = sym.name
        exprStr = hask(info.expr)
        if len(exprDepVars) == 0:
            # `maybe` expr
            ans += ident * 2 + f"me'{symName} = Just ({exprStr})\n"
        else:
            # If all exprDepVars of expr match `Just x`, the expr can be eval.
            # Otherwise, return `Nothing` means unknown value.
            # me'expr =
            #   case (mv'v1, mv'v2) of
            #     (Just v1, Just v2) -> Just (expr)
            #     _ -> Nothing
            ans += ident * 2 + f"me'{symName} =\n"
            mvs = []
            justs = []
            for depVar in exprDepVars:
                dvStr = hask(depVar)
                mvs.append("mv'" + dvStr)
                justs.append("Just " + dvStr)
            mvsStr = ", ".join(mvs)
            justsStr = ", ".join(justs)
            ans += ident * 3 + f"case ({mvsStr}) of\n"
            ans += ident * 4 + f"({justsStr}) -> Just ({exprStr})\n"
            ans += ident * 4 + f"_ -> Nothing\n"

    ans += ident + "in\n"

    # Final scale expr
    ans += ident * 2 + "expr\n"

    return ans


def hask(e: haskell.Expr) -> str:
    return haskell.haskellPrintExpr(e)
