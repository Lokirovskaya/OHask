module StatInfo where

-- Extract final stat from ExprTree

type Stat = [SFunc]

data SFunc = SFunc
  { sfuncName :: String,
    sfuncType :: String,
    sfuncExpr :: SExpr,
    sfuncParams :: [SParam]
  }

data SParam = SParam
  { sparamName :: String,
    sparamType :: String,
    sparamUnique :: String
  }

data SExpr
  = SVar
      { svarName :: String,
        svarType :: String,
        sVarUnique :: String,
        svarParams :: [String]
      }
  | SLit
      { slitValue :: String,
        slitType :: String
      }
  | SApp
      { sappExpr :: SExpr,
        sappArg :: SExpr
      }
  | SCase
      { scaseExpr :: SExpr,
        scaseAlts :: [SExpr]
      }
  | SLam
      { 
        slamParams :: [SParam],
        slamExpr :: SExpr
      }
  | SNothing