import Data.List (unfoldr)
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
    let seedl:rest = lines input
    let seedv = map (map read) $ splitEvery 2 $ drop 1 $ splitBy ' ' seedl
    let mappings = map (map (map read . splitBy ' ') . drop 1) $ splitBy "" rest
            
    let repl [o, i, r] [k, l] =
            let beg = max i k in
            let end = min (i+r) (k+l) in
            let pre = [k, beg-k] in
            let mid = [beg-i+o, end-beg] in
            let las = [end, k+l-end] in
            if end<=beg then [[], [[k, l]]] else [[mid], filter (\[a, b] -> b>0) [pre, las]]
    print $ minimum $ map head $ foldl (\a -> concat . foldl (\[p, q] n -> foldr (zipWith (++) . repl n) [p, []] q) [[], a]) seedv mappings