
{-# NOINLINE fact #-}
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
  print $ fact 10