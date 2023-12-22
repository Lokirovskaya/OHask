module ExprTreeGen (getExprNode) where

import ExprTree
import GHC.Plugins
import Util

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
        (AppExprNode $ getExprNode dflags expr')
        (AppArgNode $ getExprNode dflags arg)
    -- Lam b (Expr b)
    Lam var expr' ->
      LamNode
        (LamVarNode $ getVarInfo dflags var)
        (LamExprNode $ getExprNode dflags expr')
    -- Let (Bind b) (Expr b)
    Let bind expr' ->
      LetNode
        (LetBindNode $ getBindNode dflags bind)
        (LetExprNode $ getExprNode dflags expr')
    -- Case (Expr b) b _Type [Alt b]
    Case expr' var _ alts ->
      CaseNode
        (CaseExprNode $ getExprNode dflags expr')
        (CaseVarNode $ getVarInfo dflags var)
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
  let name = showSDoc dflags $ ppr $ GHC.Plugins.varName var
      unique = showSDoc dflags $ ppr $ GHC.Plugins.varUnique var
      kind
        | isId var = IdentKind
        | isTcTyVar var = TcTyVarKind
        | isTyVar var = TyVarKind
        | otherwise = panic "strange var kind"
      arity =
        getRuntimeArgTys (GHC.Plugins.varType var)
          |> filter (\(_, flag) -> case flag of VisArg -> True; _ -> False) -- Filter visible params
          |> length -- count
   in VarNodeInfo
        { ExprTree.varName = name,
          ExprTree.varType = showSDoc dflags $ ppr $ GHC.Plugins.varType var,
          ExprTree.varUnique = name ++ "." ++ unique,
          varKind = kind,
          varArity = arity
        }

getLitStr :: DynFlags -> Literal -> LitNodeInfo
getLitStr dflags lit =
  let litTypeStr = showSDoc dflags $ ppr $ GHC.Plugins.literalType lit
   in LitNodeInfo
        { ExprTree.litValue = showSDoc dflags $ pprLiteral id lit,
          ExprTree.litType = litTypeStr
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

getAltsNode :: DynFlags -> [Alt CoreBndr] -> [ExprNode]
getAltsNode dflags = map (getAltNode dflags)

-- Alt AltCon [b] (Expr b)
getAltNode :: DynFlags -> Alt CoreBndr -> ExprNode
getAltNode dflags (Alt guard vars expr') =
  AltNode
    (AltGuardNode OtherNode)
    (AltVarsNode $ map (VarNode . getVarInfo dflags) vars)
    (AltExprNode $ getExprNode dflags expr')