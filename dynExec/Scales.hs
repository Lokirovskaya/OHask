{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Scales where

class Len a where
  len :: a -> Maybe Int

instance Len [a] where len = Just . length

instance {-# OVERLAPS #-} Len a where len _ = Nothing

class Value a where
  value :: a -> Maybe Int

instance Value Int where value = Just

instance {-# OVERLAPS #-} Value a where value _ = Nothing

main :: IO ()
main = do
  print $ len (5 :: Int)