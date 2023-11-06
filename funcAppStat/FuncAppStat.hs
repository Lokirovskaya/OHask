{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use uncurry" #-}
module FuncAppStat (plugin) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import ExprTreeGen (getExprNode)
import ExprTreeOutput (showExprNode)
import GHC.Plugins
import StatInfo (Stat)
import StatInfoGen (genStatOfRootFunc)
import StatInfoJsonOutput (showStatInfoJson)
import StatInfoBriefOutput (showStatInfoBrief)

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
statOutputFile = "stat.json"

statBriefOutputFile :: String
statBriefOutputFile = "stat_brief.txt"

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags
  -- Clear output files
  liftIO $ writeFile treeOutputFile ""
  liftIO $ writeFile statOutputFile ""
  -- IORef of stat result
  statRef <- liftIO $ newIORef ([] :: Stat)
  -- Run plugin pass
  modguts <- bindsOnlyPass (mapM (runBind dflags statRef)) guts
  -- Output stat info
  stat <- liftIO $ readIORef statRef
  liftIO $ writeFile statOutputFile $ showStatInfoJson stat
  liftIO $ writeFile statBriefOutputFile $ showStatInfoBrief stat
  return modguts

runBind :: DynFlags -> IORef Stat -> CoreBind -> CoreM CoreBind
runBind dflags statRef bind@(NonRec bndr expr) = do
  runOneBind dflags statRef bndr expr
  return bind
runBind dflags statRef bind@(Rec bndrList) = do
  mapM_ (\(bndr, expr) -> runOneBind dflags statRef bndr expr) bndrList
  return bind

runOneBind :: DynFlags -> IORef Stat -> CoreBndr -> Expr CoreBndr -> CoreM ()
runOneBind dflags statRef bndr expr = do
  let funcName = showSDoc dflags (ppr bndr)
  let exprNode = getExprNode dflags expr
  -- tree output
  liftIO $ appendFile treeOutputFile $ "Function " ++ funcName ++ "\n"
  liftIO $ appendFile treeOutputFile $ showExprNode exprNode ++ "\n\n"
  -- get StatFunc, append into stat
  let sfuncList = genStatOfRootFunc funcName exprNode
  liftIO $ modifyIORef' statRef (++ sfuncList)
