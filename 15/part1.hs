import Data.List
import Data.Char
splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
main = do
    input <- readFile "input.txt"
    let hash = foldl (\c h -> 17*(c+ord h) `mod` 256) 0
    print $ sum $ map hash $ splitBy ',' input