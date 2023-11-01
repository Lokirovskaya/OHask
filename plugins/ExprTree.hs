{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}
module ExprTree where

import Data.Foldable (Foldable (foldl'))
import Util

data ExprInfo
  = -- Var Id
    VarInfo String
  | -- Lit Literal
    LitInfo String
  | -- App (Expr b) (Arg b)
    AppInfo ExprInfo ExprInfo
  | AppExprInfo ExprInfo
  | AppArgInfo ExprInfo
  | -- Lam b (Expr b)
    LamInfo ExprInfo ExprInfo
  | LamVarInfo String
  | LamExprInfo ExprInfo
  | -- Let (Bind b) (Expr b)
    LetInfo ExprInfo ExprInfo
  | LetBindInfo ExprInfo
  | LetExprInfo ExprInfo
  | -- NonRec b (Expr b)
    -- Rec [(b, Expr b)]
    NonRecBindInfo ExprInfo ExprInfo
  | RecBindsInfo [ExprInfo]
  | OneRecBindInfo ExprInfo ExprInfo
  | BindVarInfo String
  | BindExprInfo ExprInfo
  | -- Case (Expr b) b Type [Alt b]
    CaseInfo ExprInfo ExprInfo ExprInfo
  | CaseExprInfo ExprInfo
  | CaseVarInfo String
  | CaseAltsInfo [ExprInfo]
  | -- Alt AltCon [b] (Expr b)
    -- We only care about the final (Expr b) part
    OneCaseAltInfo ExprInfo
  | -- Cast (Expr b) CoercionR
    -- Tick CoreTickish (Expr b)
    -- Type Type
    -- Coercion Coercion
    OtherInfo

showExprInfo :: ExprInfo -> String
showExprInfo expr = showExprRec expr 0

showExprRec :: ExprInfo -> Int -> String
showExprRec expr layer
  | isOther = ""
  | innerLayerStr |> endsWith '\n' =
      indent ++ nodeName ++ parenL ++ "\n" ++ innerLayerStr ++ indent ++ parenR ++ "\n"
  | otherwise =
      indent ++ nodeName ++ parenL ++ innerLayerStr ++ parenR ++ "\n" -- Inline style
  where
    indentSize = 2
    indent = replicate (layer * indentSize) ' '
    isOther = case expr of
      OtherInfo -> True
      _ -> False
    -- if node is a list, use [ ]
    (parenL, parenR) = case expr of
      RecBindsInfo _ -> ("[", "]")
      CaseAltsInfo _ -> ("[", "]")
      _ -> ("(", ")")
    nodeName = case expr of
      VarInfo _ -> "Var"
      LitInfo _ -> "Lit"
      AppInfo _ _ -> "App"
      AppExprInfo _ -> "AppExpr"
      AppArgInfo _ -> "AppArg"
      LamInfo _ _ -> "Lam"
      LamVarInfo _ -> "LamVar"
      LamExprInfo _ -> "LamExpr"
      LetInfo _ _ -> "Let"
      LetBindInfo _ -> "LetBind"
      LetExprInfo _ -> "LetExpr"
      NonRecBindInfo _ _ -> "NonRecBind"
      RecBindsInfo _ -> "RecBinds"
      OneRecBindInfo _ _ -> "OneRecBind"
      BindVarInfo _ -> "BindVar"
      BindExprInfo _ -> "BindExpr"
      CaseInfo _ _ _ -> "Case"
      CaseExprInfo _ -> "CaseExpr"
      CaseVarInfo _ -> "CaseVar"
      CaseAltsInfo _ -> "CaseAlts"
      OneCaseAltInfo _ -> "OneCaseAlt"
      OtherInfo -> ""
    innerLayerStr = case checkNode expr of
      Leaf s -> s
      NonLeaf children -> foldl' (++) "" $ map oneChildStr children
      where
        oneChildStr e = showExprRec e (layer + 1)

-- Return data of leaf nodes, or return children of non-leaf nodes
data Node a = Leaf String | NonLeaf [a]

checkNode :: ExprInfo -> Node ExprInfo
checkNode expr =
  case expr of
    VarInfo s -> Leaf s
    LitInfo s -> Leaf s
    AppInfo e1 e2 -> NonLeaf [e1, e2]
    AppExprInfo e -> NonLeaf [e]
    AppArgInfo e -> NonLeaf [e]
    LamInfo e1 e2 -> NonLeaf [e1, e2]
    LamVarInfo s -> Leaf s
    LamExprInfo e -> NonLeaf [e]
    LetInfo e1 e2 -> NonLeaf [e1, e2]
    LetBindInfo e -> NonLeaf [e]
    LetExprInfo e -> NonLeaf [e]
    NonRecBindInfo e1 e2 -> NonLeaf [e1, e2]
    RecBindsInfo eList -> NonLeaf eList
    OneRecBindInfo e1 e2 -> NonLeaf [e1, e2]
    BindVarInfo s -> Leaf s
    BindExprInfo e -> NonLeaf [e]
    CaseInfo e1 e2 e3 -> NonLeaf [e1, e2, e3]
    CaseExprInfo e -> NonLeaf [e]
    CaseVarInfo s -> Leaf s
    CaseAltsInfo eList -> NonLeaf eList
    OneCaseAltInfo e -> NonLeaf [e]
    OtherInfo -> Leaf ""
    