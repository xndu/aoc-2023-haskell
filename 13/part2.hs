import Data.List

splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
main = do
    input <- readFile "input.txt"
    let pats = splitBy "" $ lines input
    let hrf p = maximum $ zipWith (\a b -> if 1 == length (filter id $ concat $ zipWith (zipWith (/=)) b (reverse a)) then length a else 0) (inits p) $ init $ tails p
    print $ sum (map (hrf . transpose) pats) + 100 * sum (map hrf pats)