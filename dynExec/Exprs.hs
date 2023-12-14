module Exprs where

import RandomGen

exprs = (add1, minus10, const5, add2)

add1 = \x -> x + 1
test_add1 = do
  x <- randInt
  let r = add1 x 
  return r

minus10 = \x -> x - 10

const5 = 5

add2 = \x -> \y -> x + y