import Data.Char

{-# NOINLINE (|>) #-}
(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x

{-# NOINLINE toHexStr #-}
toHexStr :: Int -> String
toHexStr x = toHexStrRev x |> reverse
  where
    toHexStrRev x'
      | x' <= 0 = ""
      | otherwise = hexBitOf (mod16 x') : toHexStrRev (div16 x')
      where 
        mod16 a = a `mod` 16
        div16 a = a `div` 16

{-# NOINLINE hexBitOf #-}
hexBitOf :: Int -> Char
hexBitOf x
  | 0 <= x && x <= 9 = chr $ x + ord '0'
  | 10 <= x && x <= 15 = chr $ (x - 10) + ord 'a'
  | otherwise = '_'
    

main :: IO ()
main = do
  print $ toHexStr 123456
