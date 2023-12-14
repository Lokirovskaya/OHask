-- Simple plugin which find all imported modules (explicit and implicit) in source file
-- Dump to stat/imports.json

module FindImports (plugin) where

import GHC.Plugins
import GHC.Tc.Types

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin {interfaceLoadAction = interfaceLoadPlugin}

interfaceLoadPlugin :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin _ iface =
  do
    dflags <- getDynFlags
    let mod = mi_module iface
    let name = showSDoc dflags (ppr mod)
    liftIO $ putStrLn $ "interface loaded: " ++ name
    return iface