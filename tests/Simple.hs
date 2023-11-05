

mmap :: (a -> b) -> [a] -> [b]
{-# NOINLINE mmap #-}
mmap _ [] = []
mmap f (x:xs) = f x : mmap f xs

{-# NOINLINE r #-}
r :: [Int]
r = mmap (1 +) [1, 2, 3]

main :: IO ()
main = do 
    print  r