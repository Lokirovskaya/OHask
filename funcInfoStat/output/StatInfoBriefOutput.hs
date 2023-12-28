module StatInfoBriefOutput (showStatInfoBrief) where

import StatInfo
import Text.Printf (printf)
import Util

showStatInfoBrief :: Stat -> String
showStatInfoBrief stat =
  concatWith "\n\n" $ map showStatFuncInfo stat

showStatFuncInfo :: SFunc -> String
showStatFuncInfo sfunc =
  printf
    "%s %s =\n  %s"
    (sfunc |> sfuncName)
    (sfunc |> sfuncParams |> map svarName |> concatWithChar ' ')
    (sfunc |> sfuncExpr |> showStatExpr)

showStatExpr :: SExpr -> String
showStatExpr expr =
  if needParen
    then "(" ++ body ++ ")"
    else body
  where
    body :: String
    body =
      case expr of
        SVarExpr (SVar {svarName = name}) -> name
        SLit value _ -> value |> squeeze '\n'
        SApp expr' arg ->
          printf
            "%s %s"
            (showStatExpr expr')
            (showStatExpr arg)
        SCase expr' alts ->
          printf
            "case %s of %s"
            (showStatExpr expr')
            (map showStatAlt alts |> concatWith " ")
        SLam params expr' ->
          printf
            "\\%s -> %s"
            (params |> map svarName |> concatWith " ")
            (showStatExpr expr')
        SNothing -> "^"
    needParen :: Bool
    needParen =
      case expr of
        SVarExpr _ -> False
        SLit _ _ -> False
        SNothing -> False
        _ -> True

showStatAlt :: SAlt -> String
showStatAlt (SAlt con vars expr) =
  if hasVars
    then
      printf
        "| %s %s -> %s"
        con
        (map svarName vars |> concatWith " ")
        (showStatExpr expr)
    else
      printf
        "| %s -> %s"
        con
        (showStatExpr expr)
  where
    hasVars :: Bool
    hasVars = not (null vars)
