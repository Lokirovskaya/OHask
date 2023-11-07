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
    "%s (%s) =\n  %s"
    (sfunc |> sfuncName)
    (sfunc |> sfuncParams |> map showStatParam |> concatWith ", ")
    (sfunc |> sfuncExpr |> showStatExpr)

showStatParam :: SParam -> String
showStatParam sparam = sparam |> sparamName

showStatExpr :: SExpr -> String
showStatExpr (SVar name _ _) = name
showStatExpr (SLit value _) = value
showStatExpr (SApp expr arg) =
  printf
    "%s(%s)"
    (showStatExpr expr)
    (showStatExpr arg)
showStatExpr (SCase expr alts) =
  printf
    "(case %s of | %s)"
    (showStatExpr expr)
    (map showStatExpr alts |> concatWith " | ")
showStatExpr SNothing = "()"
