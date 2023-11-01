module FuncAppStat (plugin) where

import Data.Foldable (Foldable (foldl'))
import ExprTree
import GHC.Core.Ppr (pprId)
import GHC.Plugins

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return $ CoreDoPluginPass "Say name" pass : todo

treeOutputFile :: String
treeOutputFile = "tree.txt"

statOutputFile :: String
statOutputFile = "stat.txt"

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags
  -- Clear output files
  liftIO $ writeFile treeOutputFile ""
  liftIO $ writeFile statOutputFile ""
  bindsOnlyPass (mapM (printBind dflags)) guts

printBind :: DynFlags -> CoreBind -> CoreM CoreBind
printBind dflags bind@(NonRec bndr expr) = do
  let funcName = showSDoc dflags (ppr bndr)
  -- tree output
  liftIO $ appendFile treeOutputFile $ "Function " ++ funcName ++ "\n"
  let exprInfo = getExprInfo dflags expr
  liftIO $ appendFile treeOutputFile $ showExprInfo exprInfo ++ "\n\n"
  -- stat output
  liftIO $ appendFile statOutputFile $ "Function " ++ funcName ++ "\n"
  liftIO $ appendFile statOutputFile $ showStats (findFuncApps exprInfo) ++ "\n\n"
  return bind
printBind _ bind = do
  return bind

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

-- Note: CoreBndr === Id === Var

getExprInfo :: DynFlags -> Expr CoreBndr -> ExprInfo
getExprInfo dflags expr =
  case expr of
    Var var -> VarInfo $ getVarStr var
    Lit lit -> LitInfo $ getLitStr lit
    App expr' arg ->
      AppInfo
        (AppArgInfo $ getExprInfo dflags arg)
        (AppExprInfo $ getExprInfo dflags expr')
    Lam var expr' ->
      LamInfo
        (LamVarInfo $ getVarStr var)
        (LamExprInfo $ getExprInfo dflags expr')
    Let bind expr' ->
      LetInfo
        (LetExprInfo $ getExprInfo dflags expr')
        (LetBindInfo $ getBindInfo bind)
    -- Case expr var _ alts -> _
    _ -> OtherInfo
  where
    getVarStr var = showSDoc dflags $ pprId var
    getLitStr lit = showSDoc dflags $ pprLiteral id lit
    -- NonRec b (Expr b)
    -- Rec [(b, Expr b)]
    getBindInfo (NonRec var expr') =
      NonRecBindInfo
        (BindVarInfo $ getVarStr var)
        (BindExprInfo $ getExprInfo dflags expr')
    getBindInfo (Rec bindList) =
      RecBindsInfo $
        map
          ( \(var, expr') ->
              OneRecBindInfo
                (BindVarInfo $ getVarStr var)
                (BindExprInfo $ getExprInfo dflags expr')
          )
          bindList

-- Find structure matches AppExpr (Var (...))
findFuncApps :: ExprInfo -> [String]
findFuncApps expr =
  case expr of
    AppExprInfo (VarInfo var) -> [var]
    _ ->
      case checkNode expr of
        Leaf _ -> []
        NonLeaf children -> foldl' (++) [] $ map findFuncApps children

showStats :: [String] -> String
showStats stat =
  "Found " ++ show (length stat) ++ " function applications\n" ++ showOneApp stat
  where
    showOneApp [] = ""
    showOneApp (app : rs) = "  " ++ app ++ "\n" ++ showOneApp rs
