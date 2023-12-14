-- Simple plugin which find all imported modules (explicit and implicit) in source file
-- Dump to stat/imports.txt

module FindImports (plugin) where

import GHC.Plugins
import GHC.Tc.Types

plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin {interfaceLoadAction = interfaceLoadPlugin}

resultOutputFile :: String
resultOutputFile = "stat/imports.txt"

interfaceLoadPlugin :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
interfaceLoadPlugin _ iface =
  do
    dflags <- getDynFlags
    let mod = mi_module iface
    let name = showSDoc dflags (ppr mod)
    -- Due to this plugin function will be invoked on each imported module,
    -- we have to use `appendFile` instead of `writeFile`.
    -- Please ensure `resultOutputFile` is empty before analysing.
    liftIO $ appendFile resultOutputFile $ name ++ "\n"
    return iface