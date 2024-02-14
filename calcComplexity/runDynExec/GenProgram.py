from typing import List, Set

from calcComplexity.runDynExec import Group
from calcComplexity.genConstraints.VarDef import VarDef, getDef
import calcComplexity.haskellStruct as haskell

ident = "  "


def genHaskellProgram(funcList: List[haskell.Func], groupList: List[Group]):
    text = showPragmasAndImports() + "\n"

    for i, group in enumerate(groupList):
        text += showGroup(group, f"g{i}") + "\n"

    with open("./tmp/dynExprs/DynExprs.hs", "w") as f:
        f.write(text)


def showPragmasAndImports() -> str:
    ans = ""
    options = [
        "{-# LANGUAGE MagicHash #-}",
        "{-# LANGUAGE EmptyCase #-}",
        "{-# LANGUAGE UnboxedTuples #-}",
        "{-# LANGUAGE RankNTypes #-}",
        "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}",
        "{-# OPTIONS_GHC -Wno-unused-imports #-}",
        "{-# OPTIONS_GHC -Wno-unused-local-binds #-}",
        "{-# OPTIONS_GHC -Wno-overlapping-patterns #-}",
        "{-# OPTIONS_GHC -Wno-missing-signatures #-}",
        "{-# OPTIONS_GHC -Wno-unused-matches #-}",
        "{-# OPTIONS_GHC -Wno-name-shadowing #-}",
        "{-# HLINT ignore #-}",
        "",
        "module DynExprs where",
    ]
    ans = "\n".join(options) + "\n\n"

    for imp in haskell.imports:
        ans += f"import qualified {imp}\n"

    return ans


def showGroup(group: Group, groupName: str) -> str:
    ans = ""

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
            varDef: VarDef = getDef(var)
            # varDepVars = (varDef.vars /\ group.domVars) - group.paramVars
            varDepVars = varDef.varSet.intersection(domVars).difference(paramVars)
            # `maybe` vars
            ans += varValueGuard(2, f"mmv'{hask(var)}", varDepVars, varDef.defStr)

    # exprs to be dynamically executed
    for sym in group.exprSymList:
        info = sym.exprInfo
        # exprDepVars = (expr.vars /\ group.domVars) - group.paramVars
        exprDepVars = info.varSet.intersection(domVars).difference(paramVars)
        mExprName = "me'" + sym.name
        exprStr = hask(info.expr)
        ans += varValueGuard(2, mExprName, exprDepVars, exprStr)

    ans += ident + "in\n"

    # Final scale expr
    ans += ident * 2 + "expr\n"

    return ans


# If all depVars has their value, the expr can be eval.
# Otherwise, return `Nothing`, i.e. the expr is invalid.
# For every var, it has 2-layer value validation check.
# A var is valid, iff:
#   1. All vars in the def of the var are valid
#   2. The var *can* produce a valid value (see VarDef.py)
# So you should do double pattern match before using a var. 
# m'expr =
#   case (m'v1, m'v2) of
#     (Just (Just v1), Just (Just v2)) -> Just (expr)
#     _ -> Nothing
def varValueGuard(
    baseIdent: int, mResultName: str, depVars: Set[haskell.Var], exprStr: str
) -> str:
    if len(depVars) == 0:
        return ident * baseIdent + f"{mResultName} = Just ({exprStr})\n"
    else:
        ans = ident * baseIdent + mResultName + " =\n"
        mdvs = []
        justs = []
        for dv in depVars:
            dvStr = hask(dv)
            mdvs.append(f"mmv'{dvStr}")
            justs.append(f"Just (Just {dvStr})")
        mvsStr = ", ".join(mdvs)
        justsStr = ", ".join(justs)
        ans += ident * (baseIdent + 1) + f"case ({mvsStr}) of\n"
        ans += ident * (baseIdent + 2) + f"({justsStr}) -> Just ({exprStr})\n"
        ans += ident * (baseIdent + 2) + f"_ -> Nothing\n"
        return ans


def hask(e: haskell.Expr) -> str:
    return haskell.haskellPrintExpr(e)
