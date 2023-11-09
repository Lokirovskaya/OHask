{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module StatInfoGen where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import FindSubFuncs
import StatInfo
import Util

-- 1. Generate StatFunc for root expr
-- 2. Find all sub-functions
-- 3. Generate StatFunc for each sub-function
-- Note: all TyConFunc filtered
genStatOfRootFunc :: String -> ExprNode -> [SFunc]
genStatOfRootFunc rootFuncName rootExpr =
  let funcList = findSubFuncList rootFuncName rootExpr
   in map genOneStatFunc funcList

genOneStatFunc :: SubFunc -> SFunc
genOneStatFunc func =
  SFunc
    { sfuncName = func |> subFuncName,
      sfuncType = func |> subFuncType,
      sfuncExpr = genStatExpr (func |> subFuncExpr) |> simplifyStatExpr,
      sfuncParams = map genStatParam $ func |> subFuncParams
    }

genStatParam :: VarNodeInfo -> SParam
genStatParam param =
  SParam
    { sparamName = param |> varName,
      sparamType = param |> varType,
      sparamUnique = param |> varUnique
    }

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
          sVarUnique = var |> varUnique,
          svarParams = var |> varParams
        }
genStatExpr (LitNode lit) =
  SLit
    { slitValue = lit |> litValue,
      slitType = lit |> litType
    }
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
genStatExpr lamNode@(LamNode _ _) =
  SLam
    { slamParams = map genStatParam $ getParamList lamNode,
      slamExpr = genStatExpr $ stripParams lamNode
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
