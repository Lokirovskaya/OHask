from .Lambda import Expr, Var, Abstr, App, UnevalSubst
from .Util import (
    currying,
    preOrderTraversal,
    getAllVars,
    getAllVarsIf,
    replaceVar,
    replaceVarDict,
)
