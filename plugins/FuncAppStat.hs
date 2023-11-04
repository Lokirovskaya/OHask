{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use uncurry" #-}
module FuncAppStat (plugin) where

import ExprTree
import GHC.Plugins
import GenFuncStatInfo

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin {installCoreToDos = install}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
  let myPass = CoreDoPluginPass "Function Application Stat" pass
  -- return $ [CoreDoPrintCore, myPass] ++ todos
  return $ myPass : todos

-- insertAfterIf :: (a -> Bool) -> a -> [a] -> [a]
-- insertAfterIf _ _ [] = []
-- insertAfterIf predict e (x : xs)
--   | predict x = x : e : xs
--   | otherwise = x : insertAfterIf predict e xs

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
  bindsOnlyPass (mapM (runBind dflags)) guts

runBind :: DynFlags -> CoreBind -> CoreM CoreBind
runBind dflags bind@(NonRec bndr expr) = do
  runOneBind dflags bndr expr
  return bind
runBind dflags bind@(Rec bndrList) = do
  mapM_ (\(bndr, expr) -> runOneBind dflags bndr expr) bndrList
  return bind

runOneBind :: DynFlags -> CoreBndr -> Expr CoreBndr -> CoreM ()
runOneBind dflags bndr expr = do
  let funcName = showSDoc dflags (ppr bndr)
  -- tree output
  liftIO $ appendFile treeOutputFile $ "Function " ++ funcName ++ "\n"
  let exprNode = getExprNode dflags expr
  liftIO $ appendFile treeOutputFile $ showExprNode exprNode ++ "\n\n"
  -- stat output
  liftIO $ appendFile statOutputFile $ showStats funcName exprNode ++ "\n\n"
