{-# NOINLINE split #-}
split :: [Int] -> ([Int], [Int])
split a =
  let len = length a
      lenL = div len 2
      l = take lenL a
      r = drop lenL a
   in (l, r)

{-# NOINLINE merge #-}
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge a [] = a
merge [] b = b
merge (x : a) (y : b)
  | x < y = x : merge a (y : b)
  | otherwise = y : merge (x : a) b

{-# NOINLINE sort #-}
sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort [x, y]
  | x > y = [y, x]
  | otherwise = [x, y]
sort a =
  let (l, r) = split a
   in merge (sort l) (sort r)

main :: IO ()
main = do
  print $ sort [1, 4, 7, 2, 5, 8, 3, 6, 9]