module StatInfo where

-- Extract final stat from ExprTree

type Stat = [SFunc]

data SFunc = SFunc
  { sfuncName :: String,
    sfuncType :: String,
    sfuncUnique :: String,
    sfuncExpr :: SExpr,
    sfuncParams :: [SVar]
  }

data SVar = SVar
  { svarName :: String,
    svarType :: String,
    svarModule :: Maybe String,
    svarUnique :: String,
    svarArity :: Int
  }

data SAlt = SAlt
  { saltCon :: String,
    saltVars :: [SVar],
    saltExpr :: SExpr
  }

data SExpr
  = SVarExpr SVar
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
        scaseAlts :: [SAlt]
      }
  | SLam
      { slamParams :: [SVar],
        slamExpr :: SExpr
      }
  | SNothing