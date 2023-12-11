module ExprTree where

data VarKind = IdentKind | TcTyVarKind | TyVarKind

data VarNodeInfo = VarNodeInfo
  { varName :: String,
    varType :: String,
    varUnique :: String,
    varKind :: VarKind,
    varArity :: Int
  }

data LitNodeInfo = LitNodeInfo
  { litValue :: String,
    litType :: String
  }

data ExprNode
  = -- Var Id
    VarNode VarNodeInfo
  | -- Lit Literal
    LitNode LitNodeInfo
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

getChildren :: ExprNode -> [ExprNode]
getChildren expr =
  case expr of
    VarNode _ -> []
    LitNode _ -> []
    AppNode e1 e2 -> [e1, e2]
    AppExprNode e -> [e]
    AppArgNode e -> [e]
    LamNode e1 e2 -> [e1, e2]
    LamVarNode _ -> []
    LamExprNode e -> [e]
    LetNode e1 e2 -> [e1, e2]
    LetBindNode e -> [e]
    LetExprNode e -> [e]
    NonRecBindNode e -> [e]
    RecBindsNode eList -> eList
    OneBindNode e1 e2 -> [e1, e2]
    BindVarNode _ -> []
    BindExprNode e -> [e]
    CaseNode e1 e2 -> [e1, e2]
    CaseExprNode e -> [e]
    CaseAltsNode eList -> eList
    OneCaseAltNode e -> [e]
    CastNode e -> [e]
    CastExprNode e -> [e]
    TickNode e -> [e]
    TickExprNode e -> [e]
    OtherNode -> []
