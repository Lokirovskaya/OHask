{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module StatInfoGen where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import FindSubFuncs
import qualified GHC.Plugins as GHC
import StatInfo
import Util

-- 1. Generate StatFunc for root expr
-- 2. Find all sub-functions
-- 3. Generate StatFunc for each sub-function
-- Note: all TyConFunc filtered
genStatOfRootFunc :: VarNodeInfo -> ExprNode -> [SFunc]
genStatOfRootFunc rootFunc rootExpr =
  let funcList = findSubFuncList rootFunc rootExpr
   in map genOneStatFunc funcList

genOneStatFunc :: SubFunc -> SFunc
genOneStatFunc func =
  SFunc
    { sfuncName = func |> subFuncName,
      sfuncType = func |> subFuncType,
      sfuncUnique = func |> subFuncUnique,
      sfuncExpr = genStatExpr (func |> subFuncExpr) |> simplifyStatExpr,
      sfuncParams = map genStatVarFromInfo (func |> subFuncParams)
    }

isTyConFunc :: String -> Bool
isTyConFunc ('$' : _ : _) = True
isTyConFunc _ = False

genStatExpr :: ExprNode -> SExpr
genStatExpr (VarNode var)
  | isTyConFunc $ var |> varName =
      SNothing
  | otherwise =
      SVarExpr $ genStatVarFromInfo var
genStatExpr (LitNode lit) =
  SLit
    { slitValue = lit |> litValue,
      slitType = lit |> litType
    }
genStatExpr (AppNode (AppExprNode appExpr) (AppArgNode arg)) =
  SApp
    { sappExpr = genStatExpr appExpr,
      sappArg = genStatExpr arg -- Could be SNothing
    }
-- ignore CaseVarNode
genStatExpr (CaseNode (CaseExprNode caseExpr) _ (CaseAltsNode caseAlts)) =
  SCase
    { scaseExpr = genStatExpr caseExpr,
      scaseAlts = map genStatAlt caseAlts
    }
genStatExpr lamNode@(LamNode _ _) =
  SLam
    { slamParams = map genStatVarFromInfo $ getParamList lamNode,
      slamExpr = genStatExpr $ stripParams lamNode
    }
genStatExpr (LetNode _ (LetExprNode expr)) =
  genStatExpr expr
genStatExpr _ =
  SNothing

genStatVarFromInfo :: VarNodeInfo -> SVar
genStatVarFromInfo var =
  SVar
    { svarName = var |> varName,
      svarType = var |> varType,
      svarModule = var |> varModule,
      svarUnique = var |> varUnique,
      svarArity = var |> varArity
    }

genStatAlt :: ExprNode -> SAlt
genStatAlt (AltNode (AltConNode con) (AltVarsNode vars) (AltExprNode expr)) =
  let castToVar :: ExprNode -> VarNodeInfo
      castToVar (VarNode var) = var
      castToVar _ = GHC.panic "Bad Var"
   in SAlt
        { saltCon = con,
          saltVars = map (genStatVarFromInfo . castToVar) vars,
          saltExpr = genStatExpr expr
        }
genStatAlt _ = GHC.panic "Bad Alt"

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
          simplAlt :: SAlt -> (SAlt, Bool)
          simplAlt alt =
            let (simplAltExpr, changeAltExpr) = simpl (saltExpr alt)
             in ( SAlt
                    { saltCon = saltCon alt,
                      saltVars = saltVars alt,
                      saltExpr = simplAltExpr
                    },
                  changeAltExpr
                )
          simplAltsResult = map simplAlt alts
          simplAlts = map fst simplAltsResult
          changeAlts = map snd simplAltsResult
          change = changeExpr || foldl' (||) False changeAlts
       in (SCase simplExpr simplAlts, change)
    simpl (SLam params expr') =
      let (simplExpr, changeExpr) = simpl expr'
       in (SLam params simplExpr, changeExpr)
    simpl expr' = (expr', False)
