module SimpleExprInfo where

import Text.Printf (printf)
import Util

-- Var Id
-- Lit Literal
-- App (Expr b) (Arg b)
-- Lam b (Expr b)
-- Let (Bind b) (Expr b)
-- Case (Expr b) b Type [Alt b]
-- Cast (Expr b) CoercionR      -- ignored
-- Tick CoreTickish (Expr b)    -- ignored
-- Type Type                    -- ignored
-- Coercion Coercion            -- ignored

type VarInfo = String

type LitInfo = String

data AppInfo = AppInfo
  { appExprInfo :: ExprInfo,
    appArgInfo :: ExprInfo
  }

data LamInfo = LamInfo
  { lamVarInfo :: VarInfo,
    lamExprInfo :: ExprInfo
  }

data LetInfo = LetInfo
  { letBindInfo :: BindInfo,
    letExprInfo :: ExprInfo
  }

data ExprInfo
  = ExprVar VarInfo
  | ExprLit LitInfo
  | ExprApp AppInfo
  | ExprLam LamInfo
  | ExprLet LetInfo
  | Other

data BindInfo
  = NonRecInfo
      { bindVarInfo :: VarInfo,
        bindExprInfo :: ExprInfo
      }
  | RecInfo

indentSize :: Int
indentSize = 2

-- 1 App(
-- 2  AppArg(
-- 3    Expr(...)
-- 4  )
-- 5  AppExpr(
-- 6    Expr(...)
-- 7  )
-- 8 )
buildFormatString :: String -> String -> String -> String -> String -> String
buildFormatString title sub1 sub2 indent moreIndent = 
  {- 1 -} indent ++ title ++ "(" ++ "\n" ++
  {- 2 -} moreIndent ++ sub1 ++ "(" ++ "\n" ++
  {- 3 -} "%s" ++
  {- 4 -} moreIndent ++ ")" ++ "\n" ++ 
  {- 5 -} moreIndent ++ sub2 ++ "(" ++ "\n" ++
  {- 6 -} "%s" ++
  {- 7 -} moreIndent ++ ")" ++ "\n" ++ 
  {- 8 -} indent ++ ")" ++ "\n"

showExprInfo :: ExprInfo -> String
showExprInfo exprInfo = prettyShow exprInfo 0
  where
    prettyShow :: ExprInfo -> Int -> String
    prettyShow expr layer =
      let indent = replicate (layer * indentSize) ' '
          moreIndent = replicate ((layer + 1) * indentSize) ' '
       in 
        case expr of
          ExprVar var -> indent ++ printf "Var(%s)\n" var
          ExprLit lit -> indent ++ printf "Lit(%s)\n" lit
          ExprApp app ->
            printf
              (buildFormatString "App" "AppArg" "AppExpr" indent moreIndent)
              (prettyShow (app |> appArgInfo) (layer + 2))
              (prettyShow (app |> appExprInfo) (layer + 2))
          ExprLam lam ->
            let formatStr =
            -- 1 Lam(
            -- 2  LamVar(...)
            -- 3  LamExpr(
            -- 4    Expr(...)
            -- 5  )
            -- 6 )
            {- 1 -} indent ++ "Lam(\n" ++
            {- 2 -} moreIndent ++ "LamVar(%s)\n" ++
            {- 3 -} moreIndent ++ "LamExpr(\n" ++
            {- 4 -} "%s" ++
            {- 5 -} moreIndent ++ ")\n" ++ 
            {- 6 -} indent ++ ")\n"
            in
              printf
                formatStr
                (lam |> lamVarInfo)
                (prettyShow (lam |> lamExprInfo) (layer + 2))
          ExprLet letInfo ->
            printf
              (buildFormatString "Let" "LetBind" "LetExpr" indent moreIndent)
              (prettyShowBind (letInfo |> letBindInfo) (layer + 2))
              (prettyShow (letInfo |> letExprInfo) (layer + 2))
          Other -> ""

    prettyShowBind :: BindInfo -> Int -> String
    prettyShowBind bindInfo layer =
      let indent = replicate (layer * indentSize) ' '
          moreIndent = replicate ((layer + 1) * indentSize) ' '
       in 
        case bindInfo of
          NonRecInfo bindVar bindExpr ->
            printf
              (buildFormatString "NonRecBind" "BindVar" "BindExpr" indent moreIndent)
              bindVar
              (prettyShow bindExpr (layer + 2))
          RecInfo -> ""
