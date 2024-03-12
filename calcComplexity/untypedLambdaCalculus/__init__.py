from .Lambda import Expr, Var, Abstr, App, Add, MaxN
from .Util import (
    currying,
    preOrderTraversal,
    getAllVars,
    getAllVarsIf,
    replaceVarDict,
)
