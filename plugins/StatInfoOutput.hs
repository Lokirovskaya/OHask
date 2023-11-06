module StatInfoOutput (showStatInfo) where

import ExprTree (showVarKind)
import StatInfo
import Text.Printf (printf)
import Util

showStatInfo :: Stat -> String
showStatInfo stat =
  printf
    "[%s]"
    (map showStatFuncInfo stat |> concatWithComma)

showStatFuncInfo :: SFunc -> String
showStatFuncInfo sfunc =
  printf
    "{\"funcName\":\"%s\",\"funcType\":\"%s\",\"funcParams\":[%s],\"funcExpr\":%s}"
    (sfunc |> sfuncName |> escape)
    (sfunc |> sfuncType |> escape)
    (sfunc |> sfuncParams |> map showStatParam |> concatWithComma)
    (sfunc |> sfuncExpr |> showStatExpr)

showStatParam :: SParam -> String
showStatParam sparam =
  printf
    "{\"paramName\":\"%s\",\"paramType\":\"%s\"}"
    (sparam |> sparamName |> escape)
    (sparam |> sparamType |> escape)

showStatExpr :: SExpr -> String
showStatExpr (SVar name type' kind) =
  printf
    "{\"exprKind\":\"Var\",\"varKind\":\"%s\",\"varName\":\"%s\",\"varType\":\"%s\"}"
    (kind |> showVarKind |> escape)
    (name |> escape)
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
    (map showStatExpr alts |> concatWithComma)
showStatExpr SNothing = "null"

concatWithComma :: [String] -> String
concatWithComma = concatWith ','

escape :: String -> String
escape "" = ""
escape ('"' : s) = "\\\"" ++ escape s
escape ('\\' : s) = "\\\\" ++ escape s
escape (c : s) = c : escape s