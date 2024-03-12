
{-# NOINLINE sumList #-}
sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

main :: IO ()
main = do
  print $ sumList [1, 2, 3, 4]