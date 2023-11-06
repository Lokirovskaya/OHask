module Util where

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x

endsWith :: (Eq a) => a -> [a] -> Bool
endsWith _ [] = False
endsWith x list = x == last list

concatWith :: Char -> [String] -> String
concatWith _ [] = ""
concatWith _ [x] = x
concatWith c (x : xs) = x ++ c : concatWith c xs
