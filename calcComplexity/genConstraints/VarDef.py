# Fill the field `haskell.Var.varDef`
# Var def expr is only determined by:
#   1. Pattern match exprs,
#        case e of
#          Con v3 v4 -> _
#      Expr above tells, of var v3, the definition is
#        case e of {Con v3 _ -> v3}
#   2. Zero-param function,
#        f = e
#      Expr above tells the definition of var f is e.

from typing import Dict, Set
import calcComplexity.haskellStruct as haskell


class VarDef:
    def __init__(self, defStr: str, varSet: Set[haskell.Var]) -> None:
        self.defStr = defStr
        self.varSet = varSet

    def __eq__(self, other: object) -> bool:
        if isinstance(other, VarDef):
            return self.defStr == other.defStr
        else:
            return False

    def __hash__(self) -> int:
        return hash(self.defStr)


defOfVar: Dict[haskell.Var, VarDef] = {}


def _addDef(var: haskell.Var, defStr: str, varSet: Set[haskell.Var]):
    assert (
        var not in defOfVar or defOfVar[var].defStr == defStr
    ), f"Conflict def of var: {var}"
    defOfVar[var] = VarDef(defStr, varSet)


def addDefExpr(var: haskell.Var, expr: haskell.Expr):
    exprStr = haskell.haskellPrintExpr(expr)
    defStr = f"Just ({exprStr})"
    varSet = haskell.getAllVars(expr)
    _addDef(var, defStr, varSet)


def addDefCase(case_: haskell.Case):
    exprStr = haskell.haskellPrintExpr(case_.caseExpr)
    varSet = haskell.getAllVars(case_.caseExpr)
    for alt in case_.caseAlts:
        if alt.altConVarCount > 0:
            for idx, var in enumerate(alt.altConVars):
                defStr = makePatternDefStr(
                    exprStr=exprStr,
                    conStr=alt.altConName,
                    conIdx=idx,
                    conCount=alt.altConVarCount,
                )
                _addDef(var, defStr, varSet)


# var = case expr of {Con t _ _ -> t}
def makePatternDefStr(exprStr: str, conStr: str, conIdx: int, conCount: int) -> str:
    tmpStr = "t"

    if conCount == 1:
        conVarsStr = tmpStr
    else:
        assert conCount > 1
        l = ["_"] * conCount
        l[conIdx] = tmpStr
        conVarsStr = " ".join(l)

    if haskell.isTupleConstructorName(conStr):
        conStr = conStr[1:-1]

    return f"case ({exprStr}) of {{({conStr}) {conVarsStr} -> Just {tmpStr}; _ -> Nothing}}"


def getDef(var: haskell.Var) -> VarDef:
    return defOfVar[var]
