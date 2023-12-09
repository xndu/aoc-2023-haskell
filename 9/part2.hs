import Data.List
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let vals = map (map read . splitBy ' ') $ lines input
    let f line = if all (==0) line then 0:line else scanl (+) (head line) $ f $ zipWith (-) (tail line) line
    print $ sum $ map (last . f . reverse) vals