import Data.List
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let [time, dist] = map (map read . filter (not . null) . drop 1 . splitBy ' ') $ lines input
    print $ product $ zipWith (\t d -> length $ filter (>d) $ map (\a -> a*(t-a)) [0..t]) time dist