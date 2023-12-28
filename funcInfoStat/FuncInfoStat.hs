{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use uncurry" #-}
module FuncInfoStat (plugin) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import ExprTree
import ExprTreeGen (getExprNode)
import ExprTreeOutput (showExprNode)
import GHC.Plugins
import StatInfo (Stat)
import StatInfoBriefOutput (showStatInfoBrief)
import StatInfoGen (genStatOfRootFunc)
import StatInfoJsonOutput (showStatInfoJson)

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin {installCoreToDos = install}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
  let myPass = CoreDoPluginPass "Function Information Stat" pass
  -- return $ [CoreDoPrintCore, myPass] ++ todos
  return $ myPass : todos

treeOutputFile :: String
treeOutputFile = "stat/tree.txt"

statOutputFile :: String
statOutputFile = "stat/stat.json"

statBriefOutputFile :: String
statBriefOutputFile = "stat/stat_brief.txt"

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
  liftIO $ writeFile statOutputFile $ showStatInfoJson $ init stat
  liftIO $ writeFile statBriefOutputFile $ showStatInfoBrief $ init stat
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
  let funcName = showSDoc dflags $ ppr $ GHC.Plugins.varName bndr
  let funcType = showSDoc dflags $ ppr $ GHC.Plugins.varType bndr
  let funcUnique = showSDoc dflags $ ppr $ GHC.Plugins.varUnique bndr
  let funcVar =
        VarNodeInfo
          { ExprTree.varName = funcName,
            ExprTree.varType = funcType,
            ExprTree.varUnique = funcName ++ "." ++ funcUnique,
            ExprTree.varModule = Just "Main",
            ExprTree.varKind = IdentKind, -- Will not be use
            ExprTree.varArity = -1 -- Will not be use
          }
  let exprNode = getExprNode dflags expr
  -- tree output
  liftIO $ appendFile treeOutputFile $ "Function " ++ funcName ++ "\n"
  liftIO $ appendFile treeOutputFile $ showExprNode exprNode ++ "\n\n"
  -- get StatFunc, append into stat
  let sfuncList = genStatOfRootFunc funcVar exprNode
  liftIO $ modifyIORef' statRef (++ sfuncList)
