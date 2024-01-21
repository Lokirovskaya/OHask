{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module FindSubFuncs where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import Util

data SubFunc = SubFunc
  { subFuncName :: String,
    subFuncType :: String,
    subFuncUnique :: String,
    subFuncParams :: [VarNodeInfo],
    subFuncExpr :: ExprNode,
    subFuncParent :: Maybe SubFunc
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
findFuncs rootFuncVar rootExpr =
  -- The root func is also a sub func
  let rootSubFunc =
        SubFunc
          { subFuncName = rootFuncVar |> varName,
            subFuncType = rootFuncVar |> varType,
            subFuncUnique = rootFuncVar |> varUnique,
            subFuncParams = getParamList rootExpr,
            subFuncExpr = stripParams rootExpr,
            subFuncParent = Nothing
          }
   in rootSubFunc : findInside (stripParams rootExpr) rootSubFunc
  where
    findInside :: ExprNode -> SubFunc -> [SubFunc]
    findInside (OneBindNode (BindVarNode var) (BindExprNode expr)) curFunc =
      let subFunc =
            SubFunc
              { subFuncName = var |> varName,
                subFuncType = var |> varType,
                subFuncUnique = var |> varUnique,
                subFuncParams = getParamList expr,
                subFuncExpr = stripParams expr,
                subFuncParent = Just curFunc
              }
       in subFunc : findInside (stripParams expr) subFunc
    findInside expr curFunc =
      foldl' (++) [] $ map (\node -> findInside node curFunc) (getChildren expr)

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
