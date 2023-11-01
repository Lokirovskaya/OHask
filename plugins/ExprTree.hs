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
    -- Cast (Expr b) CoercionR
    -- Tick CoreTickish (Expr b)
    -- Type Type
    -- Coercion Coercion
    OtherInfo

showExprInfo :: ExprInfo -> String
showExprInfo expr = showExprRec expr 0

indentSize :: Int
indentSize = 2

showExprRec :: ExprInfo -> Int -> String
showExprRec expr layer
  | isOther = ""
  | innerLayerStr |> endsWith '\n' =
      indent ++ nodeName ++ parenL ++ "\n" ++ innerLayerStr ++ indent ++ parenR ++ "\n"
  | otherwise =
      indent ++ nodeName ++ parenL ++ innerLayerStr ++ parenR ++ "\n" -- Inline style
  where
    indent = replicate (layer * indentSize) ' '
    isOther = case expr of OtherInfo -> True; _ -> False
    -- if node is a list, use [ ]
    (parenL, parenR) = case expr of RecBindsInfo _ -> ("[", "]"); _ -> ("(", ")")
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
      OtherInfo -> ""
    innerLayerStr = case expr of
      VarInfo s -> s
      LitInfo s -> s
      AppInfo e1 e2 -> expr2 e1 e2
      AppExprInfo e -> expr1 e
      AppArgInfo e -> expr1 e
      LamInfo e1 e2 -> expr2 e1 e2
      LamVarInfo s -> s
      LamExprInfo e -> expr1 e
      LetInfo e1 e2 -> expr2 e1 e2
      LetBindInfo e -> expr1 e
      LetExprInfo e -> expr1 e
      NonRecBindInfo e1 e2 -> expr2 e1 e2
      RecBindsInfo bindList -> foldl' (++) "" $ map expr1 bindList
      OneRecBindInfo e1 e2 -> expr2 e1 e2
      BindVarInfo s -> s
      BindExprInfo e -> expr1 e
      OtherInfo -> ""
      where
        expr1 e = showExprRec e (layer + 1)
        expr2 e1 e2 = showExprRec e1 (layer + 1) ++ showExprRec e2 (layer + 1)
