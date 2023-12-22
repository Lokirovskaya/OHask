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
showStatExpr (SVarExpr (SVar name _ _ _)) = name
showStatExpr (SLit value _) = value |> squeeze '\n'
showStatExpr (SApp expr arg) =
  printf
    "(%s) (%s)"
    (showStatExpr expr)
    (showStatExpr arg)
showStatExpr (SCase expr alts) =
  printf
    "(case (%s) of %s)"
    (showStatExpr expr)
    (map showStatAlt alts |> concatWith " ")
showStatExpr (SLam params expr) =
  printf
    "\\%s -> (%s)"
    (params |> map svarName |> concatWith ", ")
    (showStatExpr expr)
showStatExpr SNothing = "^"

showStatAlt :: SAlt -> String
showStatAlt alt =
  printf
    "| %s %s -> (%s)"
    (saltCon alt)
    (map svarName (saltVars alt) |> concatWith " ")
    (showStatExpr (saltExpr alt))