{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module StatInfoGen where

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
      subFuncNodeList = findSubFuncNodes (stripLambdas rootExpr)
      statSubFuncList = map (uncurry genOneStatFunc) subFuncNodeList
   in statRootFunc : statSubFuncList

-- find all sub-functions
-- 1. Manually declared sub-function: OneBind(...)
-- 2. Lambda sub-function: Lam(...)
-- return: [(subFuncName, subFuncExpr)]
findSubFuncNodes :: ExprNode -> [(String, ExprNode)]
findSubFuncNodes (OneBindNode (BindVarNode subFuncVar) (BindExprNode subFuncExpr)) =
  (subFuncVar |> varName, subFuncExpr) : findSubFuncNodes subFuncExpr
-- Lambda sub-function. Lift to root-function with unique name
findSubFuncNodes lamFunc@(LamNode _ _) =
  let innerFuncExpr = stripLambdas lamFunc
      uniqueName = getLambdaName lamFunc
   in (uniqueName, lamFunc) : findSubFuncNodes innerFuncExpr
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
      sfuncExpr = genStatExpr $ stripLambdas expr,
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

-- Strip all Lam(LamVar(...), LamExpr(...)), i.e. params part, expose function body
stripLambdas :: ExprNode -> ExprNode
stripLambdas (LamNode _ (LamExprNode expr')) = stripLambdas expr'
stripLambdas expr' = expr'

getLambdaName :: ExprNode -> String
getLambdaName expr = ".lambda" ++ getLambdaName' expr
  where
    getLambdaName' (LamNode (LamVarNode param) (LamExprNode expr')) =
      '.' : (param |> varName) ++ getLambdaName' expr'
    getLambdaName' _ = ""

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
-- Meet a lambda node. It should be lifted as a root function
-- Replace it with a named-function
genStatExpr lamNode@(LamNode _ _) =
  let lamName = getLambdaName lamNode
   in SVar
        { svarName = lamName,
          svarType = "lambda",
          svarKind = IdentKind
        }
genStatExpr (OneCaseAltNode expr) =
  genStatExpr expr
genStatExpr (LetNode (LetExprNode expr) _) =
  genStatExpr expr
genStatExpr _ =
  SNothing
