module StatInfo where

-- A function may contain many cases
-- Each case could also branch to more cases
-- At the leaf node of case-tree, there will be a func-app-dep-tree

type StatInfo = [FuncInfo]

data FuncInfo = FuncInfo
  { funcName :: String,
    funcType :: String, -- type signature
    caseRootNode :: CaseInfoNode
  }

data CaseInfoNode
  = CaseInfoLeaf FuncAppDepNode
  | CaseInfoNonLeaf
      { caseGuardExpr :: FuncAppDepNode,
        caseAlts :: [CaseInfoNode]
      }

-- Function Application Dependency Node
data FuncAppDepNode = FuncAppDepNode
  { funcAppName :: String,
    funcAppType :: String,
    funcAppArgs :: [FuncAppDepNode]
  }
