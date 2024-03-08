{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import DynExprs
import TestGroup
import Control.DeepSeq (deepseq)

resultOutputFile :: String
resultOutputFile = "stat/dyn_result.txt"

main :: IO ()
main = writeFile resultOutputFile ""