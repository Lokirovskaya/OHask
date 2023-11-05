{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module GenStatInfo where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import StatInfo
import Util

-- 1. Generate StatFunc for root expr
-- 2. Find all sub-functions
-- 3. Generate StatFunc for each sub-function
genStatOfRootFunc :: String -> ExprNode -> [SFunc]
genStatOfRootFunc rootFuncName rootExpr =
  let statRootFunc = genOneStatFunc rootFuncName rootExpr
      subFuncNodeList = findSubFuncNodes rootExpr
      statSubFuncList = map (uncurry genOneStatFunc) subFuncNodeList
   in statRootFunc : statSubFuncList

-- find all sub-functions
-- OneBind(BindVar(...))
-- return: [(subFuncName, subFuncExpr)]
findSubFuncNodes :: ExprNode -> [(String, ExprNode)]
findSubFuncNodes (OneBindNode (BindVarNode subFuncVar) (BindExprNode subFuncExpr)) =
  (subFuncVar |> varName, subFuncExpr) : findSubFuncNodes subFuncExpr
findSubFuncNodes expr =
  case checkNode expr of
    LeafLike _ -> []
    NonLeafLike children ->
      foldl' (++) [] $ map findSubFuncNodes children

genOneStatFunc :: String -> ExprNode -> SFunc
genOneStatFunc funcName expr =
  SFunc
    { sfuncName = funcName,
      sfuncType = "",
      sfuncExpr = genStatExpr expr,
      sfuncParams = findStatFuncParams expr
    }

-- If a function has params, the root node of its ExprTree should be Lam
-- Lam(LamVar(p1), LamExpr(LamVar(p2), LamExpr(...)))
findStatFuncParams :: ExprNode -> [SParam]
findStatFuncParams (LamNode (LamVarNode param) (LamExprNode expr')) =
  case param |> varKind of
    IdentKind -> SParam {sparamName = param |> varName, sparamType = ""} : findStatFuncParams expr'
    _ -> findStatFuncParams expr'
findStatFuncParams _ = []

genStatExpr :: ExprNode -> SExpr
genStatExpr (VarNode var) =
  SVar
    { svarName = var |> varName,
      svarType = var |> varType,
      svarKind = var |> varKind
    }
genStatExpr (LitNode var) =
  SVar
    { svarName = var |> varName,
      svarType = var |> varType,
      svarKind = var |> varKind
    }
-- Compress the application with no args
genStatExpr (AppNode (AppArgNode OtherNode) (AppExprNode appExpr)) =
  genStatExpr appExpr
genStatExpr (AppNode (AppArgNode arg) (AppExprNode appExpr)) =
  SApp
    { sappExpr = genStatExpr appExpr,
      sappArg = genStatExpr arg -- Could be SNothing
    }
genStatExpr (CaseNode (CaseExprNode caseExpr) (CaseAltsNode caseAlts)) =
  SCase
    { scaseExpr = genStatExpr caseExpr,
      scaseAlts = map genStatExpr caseAlts
    }
-- Skip the func params part
genStatExpr (LamNode _ (LamExprNode expr)) =
  genStatExpr expr
genStatExpr (OneCaseAltNode expr) =
  genStatExpr expr
genStatExpr _ =
  SNothing
