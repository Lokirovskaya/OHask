module RandomGen where

import Test.QuickCheck

randInt :: IO Int
randInt = generate arbitrary
