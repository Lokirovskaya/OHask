module Main where

import Data.Dynamic

class Len a where
  len :: a -> Int

instance Len Int where len _ = 1

instance Len [a] where len = length

class Value a where
  value :: a -> Int

instance Value Int where value x = x

exprs = [toDyn (1 :: Int)]

main :: IO ()
main = do
  putStrLn "DynAnalysis"
