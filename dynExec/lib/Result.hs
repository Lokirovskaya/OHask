module Result where

data ValType = IntVal | ListLen

data Value = Value {
  value :: Int,
  valType :: ValType
}

type Cell = [Value]

data Result = Result
  { resultGroupIdx :: Int,
    inputVals :: [Cell],
    outputVals :: [Cell]
  }

instance Show Value where
  show val = show (value val) ++ suffix
    where 
      suffix = 
        case valType val of
          IntVal -> ""
          ListLen -> "L"
