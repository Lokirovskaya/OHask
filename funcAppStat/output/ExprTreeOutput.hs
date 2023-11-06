module ExprTreeOutput (showExprNode, showVarKind) where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import Text.Printf (printf)
import Util

-- Pretty printer
showExprNode :: ExprNode -> String
showExprNode expr = showExprRec expr 0

showExprRec :: ExprNode -> Int -> String
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
      OtherNode -> True
      _ -> False
    -- if node is a list, use [ ]
    (parenL, parenR) = case expr of
      RecBindsNode _ -> ("[", "]")
      CaseAltsNode _ -> ("[", "]")
      _ -> ("(", ")")
    nodeName = case expr of
      VarNode _ -> "Var"
      LitNode _ -> "Lit"
      AppNode _ _ -> "App"
      AppExprNode _ -> "AppExpr"
      AppArgNode _ -> "AppArg"
      LamNode _ _ -> "Lam"
      LamVarNode _ -> "LamVar"
      LamExprNode _ -> "LamExpr"
      LetNode _ _ -> "Let"
      LetBindNode _ -> "LetBind"
      LetExprNode _ -> "LetExpr"
      NonRecBindNode _ -> "NonRecBind"
      RecBindsNode _ -> "RecBinds"
      OneBindNode _ _ -> "OneBind"
      BindVarNode _ -> "BindVar"
      BindExprNode _ -> "BindExpr"
      CaseNode _ _ -> "Case"
      CaseExprNode _ -> "CaseExpr"
      CaseAltsNode _ -> "CaseAlts"
      OneCaseAltNode _ -> "OneCaseAlt"
      CastNode _ -> "Cast"
      CastExprNode _ -> "CastExpr"
      TickNode _ -> "Tick"
      TickExprNode _ -> "TickExpr"
      OtherNode -> ""
    showVar var =
      printf
        "%s %s :: %s"
        (showVarKind $ var |> varKind)
        (var |> ExprTree.varName)
        (var |> ExprTree.varType)
    innerLayerStr = case checkNode expr of
      LeafLike var -> showVar var
      NonLeafLike children -> foldl' (++) "" $ map oneChildStr children
      where
        oneChildStr e = showExprRec e (layer + 1)

showVarKind :: VarKind -> String
showVarKind IdentKind = "Ident"
showVarKind TyVarKind = "TyVar"
showVarKind TcTyVarKind = "TcTyVa"
showVarKind LiteralKind = "Literal"