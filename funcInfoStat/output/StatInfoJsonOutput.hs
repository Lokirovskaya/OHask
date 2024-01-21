module StatInfoJsonOutput (showStatInfoJson) where

import StatInfo
import Text.Printf (printf)
import Util

showStatInfoJson :: Stat -> String
showStatInfoJson stat =
  printf
    "[%s]"
    (map showStatFuncInfo stat |> concatWithComma)

showStatFuncInfo :: SFunc -> String
showStatFuncInfo sfunc =
  printf
    "{\"funcName\":\"%s\",\"funcType\":\"%s\",\"funcUnique\":\"%s\",\"funcParentUnique\":%s,\"funcParams\":[%s],\"funcExpr\":%s}"
    (sfunc |> sfuncName |> escape)
    (sfunc |> sfuncType |> escape)
    (sfunc |> sfuncUnique |> escape)
    (strOrNull $ sfunc |> sfuncParentUnique)
    (sfunc |> sfuncParams |> map showStatVar |> concatWithComma)
    (sfunc |> sfuncExpr |> showStatExpr)

showStatVarFields :: SVar -> String
showStatVarFields
  ( SVar
      { svarName = name,
        svarType = type',
        svarModule = module',
        svarUnique = unique,
        svarArity = arity
      }
    ) =
    printf
      "\"varName\":\"%s\",\"varType\":\"%s\",\"varModule\":%s,\"varUnique\":\"%s\",\"varArity\":%d"
      (name |> escape)
      (type' |> escape)
      (strOrNull module')
      (unique |> escape)
      arity

strOrNull :: Maybe String -> String
strOrNull (Just s) = "\"" ++ s |> escape ++ "\""
strOrNull Nothing = "null"

showStatVar :: SVar -> String
showStatVar var = "{" ++ showStatVarFields var ++ "}"

showStatExpr :: SExpr -> String
showStatExpr (SVarExpr var) =
  printf
    "{\"exprKind\":\"Var\",%s}"
    (showStatVarFields var)
showStatExpr (SLit value type') =
  printf
    "{\"exprKind\":\"Lit\",\"litValue\":\"%s\",\"litType\":\"%s\"}"
    (value |> escape)
    (type' |> escape)
showStatExpr (SApp expr arg) =
  printf
    "{\"exprKind\":\"App\",\"appExpr\":%s,\"appArg\":%s}"
    (showStatExpr expr)
    (showStatExpr arg)
showStatExpr (SCase expr alts) =
  printf
    "{\"exprKind\":\"Case\",\"caseExpr\":%s,\"caseAlts\":[%s]}"
    (showStatExpr expr)
    (map showStatAlt alts |> concatWithComma)
showStatExpr (SLam params expr) =
  printf
    "{\"exprKind\":\"Lam\",\"lamParams\":[%s],\"lamExpr\":%s}"
    (map showStatVar params |> concatWithComma)
    (showStatExpr expr)
showStatExpr SNothing = "null"

showStatAlt :: SAlt -> String
showStatAlt (SAlt con vars expr) =
  printf
    "{\"caseCon\":%s,\"caseVars\":[%s],\"caseExpr\":%s}"
    conStr
    (map showStatVar vars |> concatWithComma)
    (showStatExpr expr)
  where
    conStr :: String
    conStr =
      printf
        "{\"conName\":\"%s\",\"conModule\":%s}"
        (con |> sconName |> escape)
        (strOrNull (con |> sconModule))
