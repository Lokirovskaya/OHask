{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module StatOutput where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import Text.Printf (printf)
import Util

-- 1. Find func apps in the root function
-- 2. Find all sub-functions
-- 3. Find func apps in each sub-function
showStats :: String -> ExprNode -> String
showStats rootFuncName rootExpr =
  let rootResult = printf "Function %s\n%s" rootFuncName (findFuncApps rootExpr |> prettyResult)
      subFuncList = findSubFunctions rootExpr Nothing
      oneSubFuncResult :: SubFunction -> String
      oneSubFuncResult subFunc =
        printf
          "Sub-Function %s\n%s"
          (getSubFuncFullPath subFunc)
          (findFuncApps (subFunc |> subFuncExpr) |> prettyResult)
      getSubFuncFullPath :: SubFunction -> String
      getSubFuncFullPath subFunc =
        case subFunc |> parentSubFunc of
          Nothing -> printf "%s/%s" rootFuncName (subFunc |> subFuncName)
          Just f -> printf "%s/%s" (getSubFuncFullPath f) (subFunc |> subFuncName)
      allSubFuncsResult = foldl' (\a b -> a ++ "\n" ++ b) [] $ map oneSubFuncResult subFuncList
   in rootResult ++ allSubFuncsResult

data SubFunction = SubFunction
  { subFuncName :: String,
    subFuncExpr :: ExprNode,
    -- Nothing, is an immediate sub-function of root function
    -- Just f, is an immediate sub-function of another sub-function
    parentSubFunc :: Maybe SubFunction
  }

-- find all sub-functions
-- OneBind(BindVar(...))
findSubFunctions :: ExprNode -> Maybe SubFunction -> [SubFunction]
findSubFunctions expr curSubFunc =
  case expr of
    OneBindNode (BindVarNode func) funcExpr ->
      let subFunc =
            SubFunction
              { subFuncName = extractFuncName func,
                subFuncExpr = funcExpr,
                parentSubFunc = curSubFunc
              }
       in subFunc : findSubFunctions funcExpr (Just subFunc)
    _ ->
      case checkNode expr of
        Leaf _ -> []
        NonLeaf children ->
          foldl' (++) [] $
            map
              (\childExpr -> findSubFunctions childExpr curSubFunc)
              children
  where
    -- remove the type signature manually...
    extractFuncName "" = ""
    extractFuncName (' ' : _) = ""
    extractFuncName (c : cs) = c : extractFuncName cs

-- AppExpr(Var(...))
findFuncApps :: ExprNode -> [String]
findFuncApps expr =
  case expr of
    AppExprNode (VarNode var) -> [var]
    OneBindNode _ _ -> [] -- Avoid finding func apps in sub-functions
    _ ->
      case checkNode expr of
        Leaf _ -> []
        NonLeaf children -> foldl' (++) [] $ map findFuncApps children

prettyResult :: [String] -> String
prettyResult stat =
  printf
    "(%d function applications)\n%s"
    (length stat)
    (showEachApp stat)
  where
    showEachApp [] = ""
    showEachApp (app : rs) = "  " ++ app ++ "\n" ++ showEachApp rs
