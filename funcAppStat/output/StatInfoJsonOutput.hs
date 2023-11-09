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
showStatExpr (SVar name type' params) =
  printf
    "{\"exprKind\":\"Var\",\"varName\":\"%s\",\"varType\":\"%s\",\"varParams\":[%s]}"
    (name |> escape)
    (type' |> escape)
    (params |> map (\s -> "\"" ++ s |> escape ++ "\"") |> concatWithComma)
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
    (map showStatExpr alts |> concatWithComma)
showStatExpr (SLam params expr) =
  printf
    "{\"exprKind\":\"Lam\",\"lamParams\":[%s],\"lamExpr\":%s}"
    (map showStatParam params |> concatWithComma)
    (showStatExpr expr)
showStatExpr SNothing = "null"
