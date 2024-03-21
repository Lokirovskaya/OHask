{-# NOINLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

{-# NOINLINE add1 #-}
add1 :: Int -> Int
add1 x = x + 1

main :: IO ()
main = do
  print $ (4 |> add1)