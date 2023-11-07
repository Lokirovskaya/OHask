module ExprTree where

data VarKind = IdentKind | TcTyVarKind | TyVarKind | LiteralKind

data VarNodeInfo = VarNodeInfo
  { varName :: String,
    varType :: String,
    varKind :: VarKind,
    varParams :: [String]
  }

data ExprNode
  = -- Var Id
    VarNode VarNodeInfo
  | -- Lit Literal
    LitNode VarNodeInfo
  | -- App (Expr b) (Arg b)
    AppNode ExprNode ExprNode
  | AppExprNode ExprNode
  | AppArgNode ExprNode
  | -- Lam b (Expr b)
    LamNode ExprNode ExprNode
  | LamVarNode VarNodeInfo
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
  | BindVarNode VarNodeInfo
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

-- Node-like representation of ExprNode
data NodeLike = LeafLike VarNodeInfo | NonLeafLike [ExprNode]

checkNode :: ExprNode -> NodeLike
checkNode expr =
  case expr of
    VarNode s -> LeafLike s
    LitNode s -> LeafLike s
    AppNode e1 e2 -> NonLeafLike [e1, e2]
    AppExprNode e -> NonLeafLike [e]
    AppArgNode e -> NonLeafLike [e]
    LamNode e1 e2 -> NonLeafLike [e1, e2]
    LamVarNode s -> LeafLike s
    LamExprNode e -> NonLeafLike [e]
    LetNode e1 e2 -> NonLeafLike [e1, e2]
    LetBindNode e -> NonLeafLike [e]
    LetExprNode e -> NonLeafLike [e]
    NonRecBindNode e -> NonLeafLike [e]
    RecBindsNode eList -> NonLeafLike eList
    OneBindNode e1 e2 -> NonLeafLike [e1, e2]
    BindVarNode s -> LeafLike s
    BindExprNode e -> NonLeafLike [e]
    CaseNode e1 e2 -> NonLeafLike [e1, e2]
    CaseExprNode e -> NonLeafLike [e]
    CaseAltsNode eList -> NonLeafLike eList
    OneCaseAltNode e -> NonLeafLike [e]
    CastNode e -> NonLeafLike [e]
    CastExprNode e -> NonLeafLike [e]
    TickNode e -> NonLeafLike [e]
    TickExprNode e -> NonLeafLike [e]
    OtherNode -> NonLeafLike []
