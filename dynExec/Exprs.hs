module Exprs where

import RandomGen

class Len a where
  len :: a -> Int

instance Len Int where
  len x = x

add1 = \x -> x + 1
test_add1 = do
  x <- randInt
  let r = add1 x 
  return $ len r

minus10 = \x -> x - 10

const5 = 5

add2 = \x -> \y -> x + y