
f :: Int -> Int
f x = x + 1


main :: IO ()
main = do
    let x = 3 in 
       print $ f $ x + 3