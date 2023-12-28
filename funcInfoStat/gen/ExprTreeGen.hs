module ExprTreeGen (getExprNode) where

import ExprTree
import qualified GHC.Plugins as GHC
import Util

-- Fetch useful AST nodes from GHC Core
-- Note: CoreBndr === Id === Var
getExprNode :: GHC.DynFlags -> GHC.Expr GHC.CoreBndr -> ExprNode
getExprNode dflags expr =
  case expr of
    -- Var Id
    GHC.Var var -> VarNode $ getVarInfo dflags var
    -- Lit Literal
    GHC.Lit lit -> LitNode $ getLitStr dflags lit
    -- App (Expr b) (Arg b)
    GHC.App expr' arg ->
      AppNode
        (AppExprNode $ getExprNode dflags expr')
        (AppArgNode $ getExprNode dflags arg)
    -- Lam b (Expr b)
    GHC.Lam var expr' ->
      LamNode
        (LamVarNode $ getVarInfo dflags var)
        (LamExprNode $ getExprNode dflags expr')
    -- Let (Bind b) (Expr b)
    GHC.Let bind expr' ->
      LetNode
        (LetBindNode $ getBindNode dflags bind)
        (LetExprNode $ getExprNode dflags expr')
    -- Case (Expr b) b _Type [Alt b]
    GHC.Case expr' var _ alts ->
      CaseNode
        (CaseExprNode $ getExprNode dflags expr')
        (CaseVarNode $ getVarInfo dflags var)
        (CaseAltsNode $ getAltsNode dflags alts)
    -- Cast (Expr b) _CoercionR
    GHC.Cast expr' _ ->
      CastNode $ CastExprNode $ getExprNode dflags expr'
    -- Tick _CoreTickish (Expr b)
    GHC.Tick _ expr' ->
      TickNode $ TickExprNode $ getExprNode dflags expr'
    -- Type Type
    -- Coercion Coercion
    _ -> OtherNode

getVarInfo :: GHC.DynFlags -> GHC.Var -> VarNodeInfo
getVarInfo dflags var =
  let name = GHC.showSDoc dflags $ GHC.ppr $ GHC.varName var
      type' = GHC.showSDoc dflags $ GHC.ppr $ GHC.varType var
      realUnique = GHC.showSDoc dflags $ GHC.ppr $ GHC.varUnique var
      module' = GHC.showSDoc dflags . GHC.ppr <$> GHC.nameModule_maybe (GHC.varName var)
      kind
        | GHC.isId var = IdentKind
        | GHC.isTcTyVar var = TcTyVarKind
        | GHC.isTyVar var = TyVarKind
        | otherwise = GHC.panic "strange var kind"
      arity =
        GHC.getRuntimeArgTys (GHC.varType var)
          |> filter (\(_, flag) -> case flag of GHC.VisArg -> True; _ -> False) -- Filter visible params
          |> length -- count
   in VarNodeInfo
        { varName = name,
          varType = type',
          varUnique = name ++ "." ++ realUnique,
          varModule = module',
          varKind = kind,
          varArity = arity
        }

getLitStr :: GHC.DynFlags -> GHC.Literal -> LitNodeInfo
getLitStr dflags lit =
  let litTypeStr = GHC.showSDoc dflags $ GHC.ppr $ GHC.literalType lit
   in LitNodeInfo
        { litValue = GHC.showSDoc dflags $ GHC.pprLiteral id lit,
          litType = litTypeStr
        }

-- NonRec b (Expr b)
-- Rec [(b, Expr b)]
getBindNode :: GHC.DynFlags -> GHC.Bind GHC.Var -> ExprNode
getBindNode dflags (GHC.NonRec var expr') =
  NonRecBindNode $
    OneBindNode
      (BindVarNode $ getVarInfo dflags var)
      (BindExprNode $ getExprNode dflags expr')
getBindNode dflags (GHC.Rec bindList) =
  RecBindsNode $
    map
      ( \(var, expr') ->
          OneBindNode
            (BindVarNode $ getVarInfo dflags var)
            (BindExprNode $ getExprNode dflags expr')
      )
      bindList

getAltsNode :: GHC.DynFlags -> [GHC.Alt GHC.CoreBndr] -> [ExprNode]
getAltsNode dflags = map (getAltNode dflags)

-- Alt AltCon [b] (Expr b)
getAltNode :: GHC.DynFlags -> GHC.Alt GHC.CoreBndr -> ExprNode
getAltNode dflags (GHC.Alt con vars expr') =
  AltNode
    (AltConNode $ GHC.showSDoc dflags $ GHC.ppr con)
    (AltVarsNode $ map (VarNode . getVarInfo dflags) vars)
    (AltExprNode $ getExprNode dflags expr')