module Result where

import Util
import Text.Printf (printf)


data Value = Value
  { value :: Int,
    valType :: String
  }

type Cell = [Value]

data Result = Result
  { groupIdx :: Int,
    inputVals :: [Cell],
    outputVals :: [Cell]
  }

instance Show Value where
  show val =
    printf
      "(%s,%d)"
      (valType val)
      (value val)

instance Show Result where
  show res =
    let groupName = "g" ++ show (groupIdx res)
        inputs = concatWithChar ' ' (map show (inputVals res))
        outputs = concatWithChar ' ' (map show (outputVals res))
     in "$ " ++ groupName ++ "\n> " ++ inputs ++ "\n< " ++ outputs ++ "\n"
