module FindSubFuncs where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import Util

data SubFunc = SubFunc
  { subFuncName :: String,
    subFuncType :: String,
    subFuncUnique :: String,
    subFuncParams :: [VarNodeInfo],
    subFuncExpr :: ExprNode
  }

-- find all sub-functions, filter type construction func
findSubFuncList :: VarNodeInfo -> ExprNode -> [SubFunc]
findSubFuncList rootFunc rootExpr = filterTyCon $ findFuncs rootFunc rootExpr
  where
    filterTyCon = filter (\f -> not $ isTyConFunc $ f |> subFuncName)
    isTyConFunc ('$' : _ : _) = True
    isTyConFunc _ = False

-- find all sub-functions
-- OneBind(...)
findFuncs :: VarNodeInfo -> ExprNode -> [SubFunc]
findFuncs rootFunc rootExpr =
  -- The root func is also a sub func
  SubFunc
    { subFuncName = rootFunc |> varName,
      subFuncType = rootFunc |> varType,
      subFuncUnique = rootFunc |> varUnique,
      subFuncParams = getParamList rootExpr,
      subFuncExpr = stripParams rootExpr
    }
    : findInside (stripParams rootExpr)
  where
    findInside :: ExprNode -> [SubFunc]
    findInside (OneBindNode (BindVarNode var) (BindExprNode expr)) =
      SubFunc
        { subFuncName = var |> varName,
          subFuncType = var |> varType,
          subFuncUnique = var |> varUnique,
          subFuncParams = getParamList expr,
          subFuncExpr = stripParams expr
        }
        : findInside (stripParams expr)
    findInside expr =
      foldl' (++) [] $ map findInside (getChildren expr)

-- If a function has params, the root node of its ExprTree should be Lam
-- Lam(LamVar(p1), LamExpr(LamVar(p2), LamExpr(...)))
getParamList :: ExprNode -> [VarNodeInfo]
getParamList (LamNode (LamVarNode param) (LamExprNode expr)) =
  case param |> varKind of
    IdentKind -> param : getParamList expr
    _ -> getParamList expr
getParamList _ = []

-- Strip all Lam(LamVar(...), LamExpr(...)), i.e. params part, expose function body
stripParams :: ExprNode -> ExprNode
stripParams (LamNode _ (LamExprNode expr)) = stripParams expr
stripParams expr = expr
