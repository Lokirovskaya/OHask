import Data.Char

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x

toHexStr :: Int -> String
toHexStr x = toHexStrRev x |> reverse
  where
    toHexStrRev x'
      | x' <= 0 = ""
      | otherwise = hexBitOf (x' `mod` 16) : toHexStrRev (x' `div` 16)

hexBitOf :: Int -> Char
hexBitOf x
  | 0 <= x && x <= 9 = chr $ x + ord '0'
  | 10 <= x && x <= 15 = chr $ (x - 10) + ord 'a'
  | otherwise = '_'

main :: IO ()
main = do
  print $ toHexStr 123456
