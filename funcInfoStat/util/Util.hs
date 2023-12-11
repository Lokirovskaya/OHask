module Util where

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x

endsWith :: (Eq a) => a -> [a] -> Bool
endsWith _ [] = False
endsWith x list = x == last list

concatWith :: String -> [String] -> String
concatWith _ [] = ""
concatWith _ [x] = x
concatWith s (x : xs) = x ++ s ++ concatWith s xs

concatWithChar :: Char -> [String] -> String
concatWithChar _ [] = ""
concatWithChar _ [x] = x
concatWithChar c (x : xs) = x ++ c : concatWithChar c xs

concatWithComma :: [String] -> String
concatWithComma = concatWithChar ','

escape :: String -> String
escape "" = ""
escape ('"' : s) = "\\\"" ++ escape s
escape ('\\' : s) = "\\\\" ++ escape s
escape ('\n' : s) = escape s
escape (c : s) = c : escape s

squeeze :: Char -> String -> String
squeeze _ "" = ""
squeeze c (x : s)
  | c == x = squeeze c s
  | otherwise = x : squeeze c s