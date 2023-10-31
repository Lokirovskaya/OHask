module FuncAppStat (plugin) where

import GHC.Core.Ppr (pprId)
import GHC.Plugins
import SimpleExprInfo

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
    Var var -> ExprVar $ getVarStr var
    Lit lit -> ExprLit $ getLitStr lit
    App expr' arg ->
      ExprApp $
        AppInfo
          { appArgInfo = getExprInfo dflags arg,
            appExprInfo = getExprInfo dflags expr'
          }
    Lam var expr' ->
      ExprLam $
        LamInfo
          { lamVarInfo = getVarStr var,
            lamExprInfo = getExprInfo dflags expr'
          }
    Let bind expr' ->
      ExprLet $
        LetInfo
          { letExprInfo =
              getExprInfo dflags expr',
            letBindInfo = getBindInfo bind
          }
    _ -> Other
  where
    getVarStr var = showSDoc dflags $ pprId var
    getLitStr lit = showSDoc dflags $ pprLiteral id lit
    getBindInfo (NonRec var expr') =
      NonRecInfo
        { bindVarInfo = getVarStr var,
          bindExprInfo = getExprInfo dflags expr'
        }
    getBindInfo (Rec _) = RecInfo

-- Find structure matches AppExpr (Var (...))
findFuncApps :: ExprInfo -> [String]
findFuncApps (ExprApp (AppInfo (ExprVar var) _)) = [var]
findFuncApps (ExprApp (AppInfo expr arg)) = findFuncApps expr ++ findFuncApps arg
findFuncApps (ExprLam (LamInfo _ expr)) = findFuncApps expr
findFuncApps (ExprLet (LetInfo bind expr)) = findFuncApps expr
findFuncApps _ = []

showStats :: [String] -> String
showStats stat =
  "Found " ++ show (length stat) ++ " function applications\n" ++ showOneApp stat
  where
    showOneApp [] = ""
    showOneApp (app : rs) = "  " ++ app ++ "\n" ++ showOneApp rs