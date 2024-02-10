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

import calcComplexity.haskellStruct as haskell


def addDefExpr(var: haskell.Var):
    pass
