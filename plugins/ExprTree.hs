module ExprTree where

import Data.Foldable (Foldable (foldl'))
import GHC.Core.Ppr (pprId)
import GHC.Plugins
import Util

data ExprNode
  = -- Var Id
    VarNode String
  | -- Lit Literal
    LitNode String
  | -- App (Expr b) (Arg b)
    AppNode ExprNode ExprNode
  | AppExprNode ExprNode
  | AppArgNode ExprNode
  | -- Lam b (Expr b)
    LamNode ExprNode ExprNode
  | LamVarNode String
  | LamExprNode ExprNode
  | -- Let (Bind b) (Expr b)
    LetNode ExprNode ExprNode
  | LetBindNode ExprNode
  | LetExprNode ExprNode
  | -- NonRec b (Expr b)
    -- Rec [(b, Expr b)]
    NonRecBindNode ExprNode
  | RecBindsNode [ExprNode]
  | OneBindNode ExprNode ExprNode
  | BindVarNode String
  | BindExprNode ExprNode
  | -- Case (Expr b) _b _Type [Alt b]
    CaseNode ExprNode ExprNode
  | CaseExprNode ExprNode
  | CaseAltsNode [ExprNode]
  | -- Alt _AltCon _[b] (Expr b)
    -- We only care about the final (Expr b) part
    OneCaseAltNode ExprNode
  | -- Cast (Expr b) _CoercionR
    CastNode ExprNode
  | CastExprNode ExprNode
  | -- Tick _CoreTickish (Expr b)
    TickNode ExprNode
  | TickExprNode ExprNode
  | -- Type Type
    -- Coercion Coercion
    OtherNode

-- Fetch useful AST nodes from GHC Core
-- Note: CoreBndr === Id === Var
getExprNode :: DynFlags -> Expr CoreBndr -> ExprNode
getExprNode dflags expr =
  case expr of
    -- Var Id
    Var var -> VarNode $ getVarStr var
    -- Lit Literal
    Lit lit -> LitNode $ getLitStr lit
    -- App (Expr b) (Arg b)
    App expr' arg ->
      AppNode
        (AppArgNode $ getExprNode dflags arg)
        (AppExprNode $ getExprNode dflags expr')
    -- Lam b (Expr b)
    Lam var expr' ->
      LamNode
        (LamVarNode $ getVarStr var)
        (LamExprNode $ getExprNode dflags expr')
    -- Let (Bind b) (Expr b)
    Let bind expr' ->
      LetNode
        (LetExprNode $ getExprNode dflags expr')
        (LetBindNode $ getBindNode bind)
    -- Case (Expr b) _b _Type [Alt b]
    Case expr' _ _ alts ->
      CaseNode
        (CaseExprNode $ getExprNode dflags expr')
        (CaseAltsNode $ getAltsNode alts)
    -- Cast (Expr b) _CoercionR
    Cast expr' _ ->
      CastNode $ CastExprNode $ getExprNode dflags expr'
    -- Tick _CoreTickish (Expr b)
    Tick _ expr' ->
      TickNode $ TickExprNode $ getExprNode dflags expr'
    -- Type Type
    -- Coercion Coercion
    _ -> OtherNode
  where
    getVarStr var = showSDoc dflags $ pprId var
    getLitStr lit = showSDoc dflags $ pprLiteral id lit
    -- NonRec b (Expr b)
    -- Rec [(b, Expr b)]
    getBindNode :: Bind Var -> ExprNode
    getBindNode (NonRec var expr') =
      NonRecBindNode $ OneBindNode
        (BindVarNode $ getVarStr var)
        (BindExprNode $ getExprNode dflags expr')
    getBindNode (Rec bindList) =
      RecBindsNode $
        map
          ( \(var, expr') ->
              OneBindNode
                (BindVarNode $ getVarStr var)
                (BindExprNode $ getExprNode dflags expr')
          )
          bindList
    -- Alt AltCon [b] (Expr b)
    -- We only care about the final (Expr b) part
    getAltsNode :: [Alt Var] -> [ExprNode]
    getAltsNode =
      map (\(Alt _ _ expr') -> OneCaseAltNode $ getExprNode dflags expr')

-- Return data of leaf nodes, or return children of non-leaf nodes
data Node a = Leaf String | NonLeaf [a]

checkNode :: ExprNode -> Node ExprNode
checkNode expr =
  case expr of
    VarNode s -> Leaf s
    LitNode s -> Leaf s
    AppNode e1 e2 -> NonLeaf [e1, e2]
    AppExprNode e -> NonLeaf [e]
    AppArgNode e -> NonLeaf [e]
    LamNode e1 e2 -> NonLeaf [e1, e2]
    LamVarNode s -> Leaf s
    LamExprNode e -> NonLeaf [e]
    LetNode e1 e2 -> NonLeaf [e1, e2]
    LetBindNode e -> NonLeaf [e]
    LetExprNode e -> NonLeaf [e]
    NonRecBindNode e -> NonLeaf [e]
    RecBindsNode eList -> NonLeaf eList
    OneBindNode e1 e2 -> NonLeaf [e1, e2]
    BindVarNode s -> Leaf s
    BindExprNode e -> NonLeaf [e]
    CaseNode e1 e2 -> NonLeaf [e1, e2]
    CaseExprNode e -> NonLeaf [e]
    CaseAltsNode eList -> NonLeaf eList
    OneCaseAltNode e -> NonLeaf [e]
    CastNode e -> NonLeaf [e]
    CastExprNode e -> NonLeaf [e]
    TickNode e -> NonLeaf [e]
    TickExprNode e -> NonLeaf [e]
    OtherNode -> Leaf ""

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
    innerLayerStr = case checkNode expr of
      Leaf s -> s
      NonLeaf children -> foldl' (++) "" $ map oneChildStr children
      where
        oneChildStr e = showExprRec e (layer + 1)