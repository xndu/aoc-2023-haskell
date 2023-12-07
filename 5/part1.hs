import Data.List (unfoldr)
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let seedl:rest = lines input
    let seedv = map read $ drop 1 $ splitBy ' ' seedl
    let mappings = map (map (map read . splitBy ' ') . drop 1) $ splitBy "" rest
    print $ minimum $ foldl (\p m -> map (\n -> foldl (\q [o, i, r] -> if i<=n && n<i+r then n-i+o else q) n m) p) seedv mappings