module RandomGen where


import Test.QuickCheck

randInt :: IO Int
randInt = generate arbitrary

randIntList :: IO [Int]
randIntList = generate arbitrary
