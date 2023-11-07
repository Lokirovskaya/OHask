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
      sfuncExpr = genStatExpr (stripLambdas expr) |> simplifyStatExpr,
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

getLambdaParams :: ExprNode -> [VarNodeInfo]
getLambdaParams (LamNode (LamVarNode param) (LamExprNode expr')) =
  param : getLambdaParams expr'
getLambdaParams _ = []

getLambdaName :: ExprNode -> String
getLambdaName expr =
  ".lambda." ++ (getLambdaParams expr |> map varName |> concatWithChar '.')

getLambdaParamTypes :: ExprNode -> [String]
getLambdaParamTypes expr = getLambdaParams expr |> map varType

isTyConFunc :: String -> Bool
isTyConFunc ('$' : _ : _) = True
isTyConFunc _ = False

genStatExpr :: ExprNode -> SExpr
genStatExpr (VarNode var)
  | isTyConFunc $ var |> varName =
      SNothing
  | otherwise =
      SVar
        { svarName = var |> varName,
          svarType = var |> varType,
          svarParams = var |> varParams
        }
genStatExpr (LitNode var) =
  SLit
    { slitValue = var |> varName,
      slitType = var |> varType
    }
-- -- Compress the application with no args
-- genStatExpr (AppNode (AppArgNode OtherNode) (AppExprNode appExpr)) =
--   genStatExpr appExpr
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
  SVar
    { svarName = getLambdaName lamNode,
      svarType = "(lambda)",
      svarParams = getLambdaParamTypes lamNode
    }
genStatExpr (OneCaseAltNode expr) =
  genStatExpr expr
genStatExpr (LetNode (LetExprNode expr) _) =
  genStatExpr expr
genStatExpr _ =
  SNothing

-- Simplify until no change occurs
simplifyStatExpr :: SExpr -> SExpr
simplifyStatExpr expr =
  let (s, change) = simpl expr
   in if change then simplifyStatExpr s else s
  where
    simpl :: SExpr -> (SExpr, Bool)
    -- App, but expr is null
    simpl (SApp SNothing _) = (SNothing, True)
    -- App, but arg is null
    simpl (SApp expr' SNothing) = simpl expr'
    -- Recursively simplify
    simpl (SApp expr' arg) =
      let (simplExpr, changeExpr) = simpl expr'
          (simplArg, changeArg) = simpl arg
          change = changeExpr || changeArg
       in (SApp simplExpr simplArg, change)
    simpl (SCase expr' alts) =
      let (simplExpr, changeExpr) = simpl expr'
          ss = map simpl alts
          simplAlts = map fst ss
          changeAlts = map snd ss
          change = changeExpr || foldl' (||) False changeAlts
       in (SCase simplExpr simplAlts, change)
    simpl expr' = (expr', False)
