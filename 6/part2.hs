import Data.List
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let [t, d] = map (read . concat . drop 1 . splitBy ' ') $ lines input
    print $ length $ filter (>d) $ map (\a -> a*(t-a)) [0..t]