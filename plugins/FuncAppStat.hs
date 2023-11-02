module FuncAppStat (plugin) where

import ExprTree
import GHC.Plugins
import StatOutput

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin {installCoreToDos = install}

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
  let exprNode = getExprNode dflags expr
  liftIO $ appendFile treeOutputFile $ showExprNode exprNode ++ "\n\n"
  -- stat output
  liftIO $ appendFile statOutputFile $ showStats funcName exprNode ++ "\n\n"
  return bind
printBind _ bind = do
  return bind
