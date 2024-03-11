{-# LANGUAGE FlexibleInstances #-}

module RandomGen where

import Test.QuickCheck

class Rand a where
  rand :: IO a

instance Rand Int where
  rand = generate (resize 500 arbitrary)

instance Rand [Int] where
  rand = generate (resize 200 arbitrary)
