module StatInfoOutput (showStatInfo) where

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
    "{funcName:\"%s\",funcType:\"%s\",funcParams:[%s],funcExpr:%s}"
    (sfunc |> sfuncName)
    (sfunc |> sfuncType)
    (sfunc |> sfuncParams |> map showStatParam |> concatWithComma)
    (sfunc |> sfuncExpr |> showStatExpr)

showStatParam :: SParam -> String
showStatParam sparam =
  printf
    "{paramName:\"%s\",paramType:\"%s\"}"
    (sparam |> sparamName)
    (sparam |> sparamType)

showStatExpr :: SExpr -> String
showStatExpr (SVar name type') =
  printf
    "{varName:\"%s\",varType:\"%s\"}"
    name
    type'
showStatExpr (SApp expr arg) =
  printf
    "{appExpr:%s,appArg:%s}"
    (showStatExpr expr)
    (showStatExpr arg)
showStatExpr (SCase expr alts) =
  printf
    "{caseExpr:%s,caseAlts:[%s]}"
    (showStatExpr expr)
    (map showStatExpr alts |> concatWithComma)
showStatExpr SNothing = "null"

concatWithComma :: [String] -> String
concatWithComma [] = ""
concatWithComma [x, l] = x ++ "," ++ l
concatWithComma (x : xs) = x ++ "," ++ concatWithComma xs