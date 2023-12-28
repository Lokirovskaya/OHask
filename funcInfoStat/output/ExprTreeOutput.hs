module ExprTreeOutput (showExprNode, showVarKind) where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import Text.Printf (printf)
import Util
import Data.Maybe (fromMaybe)

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
      AltVarsNode _ -> ("[", "]")
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
      CaseNode {} -> "Case"
      CaseVarNode _ -> "CaseVar"
      CaseExprNode _ -> "CaseExpr"
      CaseAltsNode _ -> "CaseAlts"
      AltNode {} -> "Alt"
      AltConNode _ -> "AltCon"
      AltVarsNode _ -> "AltVars"
      AltExprNode _ -> "AltExpr"
      CastNode _ -> "Cast"
      CastExprNode _ -> "CastExpr"
      TickNode _ -> "Tick"
      TickExprNode _ -> "TickExpr"
      OtherNode -> ""
    showVar :: VarNodeInfo -> String
    showVar var =
      printf
        "%s.%s :: %s, kind: %s, unique: %s, arity: %d"
        (fromMaybe "" (ExprTree.varModule var))
        (var |> ExprTree.varName)
        (var |> ExprTree.varType)
        (var |> varKind |> showVarKind)
        (var |> ExprTree.varUnique)
        (var |> ExprTree.varArity)
    showLit :: LitNodeInfo -> String
    showLit lit =
      printf
        "%s :: %s"
        (lit |> ExprTree.litValue)
        (lit |> ExprTree.litType)
    innerLayerStr =
      case expr of
        VarNode var -> showVar var
        LitNode lit -> showLit lit
        LamVarNode var -> showVar var
        BindVarNode var -> showVar var
        CaseVarNode var -> showVar var
        AltConNode s -> s
        OtherNode -> ""
        _ -> foldl' (++) "" $ map oneChildStr (getChildren expr)
      where
        oneChildStr e = showExprRec e (layer + 1)

showVarKind :: VarKind -> String
showVarKind IdentKind = "Ident"
showVarKind TyVarKind = "TyVar"
showVarKind TcTyVarKind = "TcTyVa"