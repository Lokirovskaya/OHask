module ExprTreeGen (getExprNode) where

import ExprTree
import GHC.Plugins

-- Fetch useful AST nodes from GHC Core
-- Note: CoreBndr === Id === Var
getExprNode :: DynFlags -> Expr CoreBndr -> ExprNode
getExprNode dflags expr =
  case expr of
    -- Var Id
    Var var -> VarNode $ getVarInfo dflags var
    -- Lit Literal
    Lit lit -> LitNode $ getLitStr dflags lit
    -- App (Expr b) (Arg b)
    App expr' arg ->
      AppNode
        (AppArgNode $ getExprNode dflags arg)
        (AppExprNode $ getExprNode dflags expr')
    -- Lam b (Expr b)
    Lam var expr' ->
      LamNode
        (LamVarNode $ getVarInfo dflags var)
        (LamExprNode $ getExprNode dflags expr')
    -- Let (Bind b) (Expr b)
    Let bind expr' ->
      LetNode
        (LetExprNode $ getExprNode dflags expr')
        (LetBindNode $ getBindNode dflags bind)
    -- Case (Expr b) _b _Type [Alt b]
    Case expr' _ _ alts ->
      CaseNode
        (CaseExprNode $ getExprNode dflags expr')
        (CaseAltsNode $ getAltsNode dflags alts)
    -- Cast (Expr b) _CoercionR
    Cast expr' _ ->
      CastNode $ CastExprNode $ getExprNode dflags expr'
    -- Tick _CoreTickish (Expr b)
    Tick _ expr' ->
      TickNode $ TickExprNode $ getExprNode dflags expr'
    -- Type Type
    -- Coercion Coercion
    _ -> OtherNode

getVarInfo :: DynFlags -> Var -> VarNodeInfo
getVarInfo dflags var =
  let kind
        | isId var = IdentKind
        | isTcTyVar var = TcTyVarKind
        | isTyVar var = TyVarKind
        | otherwise = panic "strange var kind"
   in VarNodeInfo
        { ExprTree.varName = showSDoc dflags $ ppr $ GHC.Plugins.varName var,
          ExprTree.varType = showSDoc dflags $ ppr $ GHC.Plugins.varType var,
          varKind = kind
        }

getLitStr :: DynFlags -> Literal -> VarNodeInfo
getLitStr dflags lit =
  VarNodeInfo
    { ExprTree.varName = showSDoc dflags $ pprLiteral id lit,
      ExprTree.varType = showSDoc dflags $ ppr $ GHC.Plugins.literalType lit,
      varKind = LiteralKind
    }

-- NonRec b (Expr b)
-- Rec [(b, Expr b)]
getBindNode :: DynFlags -> Bind Var -> ExprNode
getBindNode dflags (NonRec var expr') =
  NonRecBindNode $
    OneBindNode
      (BindVarNode $ getVarInfo dflags var)
      (BindExprNode $ getExprNode dflags expr')
getBindNode dflags (Rec bindList) =
  RecBindsNode $
    map
      ( \(var, expr') ->
          OneBindNode
            (BindVarNode $ getVarInfo dflags var)
            (BindExprNode $ getExprNode dflags expr')
      )
      bindList

-- Alt AltCon [b] (Expr b)
-- We only care about the final (Expr b) part
getAltsNode :: DynFlags -> [Alt CoreBndr] -> [ExprNode]
getAltsNode dflags =
  map (\(Alt _ _ expr') -> OneCaseAltNode $ getExprNode dflags expr')
