import Data.List

main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let qnt sep = intercalate sep . replicate 5
    let xt k = (++repeat k)
    let ck w = scanr (\m n ->
                scanr (
                    \(p, a) b -> if length a<m then 0 else let (c,d) = splitAt m a in
                        (if head a/='#' then b else 0) +
                        if '.' `elem` c || Just 0 == '#' `elemIndex` d then 0 else p
                ) 0 $ zip (xt 0 $ drop (m+1) n) $ init $ tails w
            ) $ xt 1 $ map (\t -> if '#' `elem` t then 0 else 1) $ tails w
    print $ sum $ map ((\[line, nums] -> head $ head $ qnt "?" line `ck` qnt [] (map read $ splitBy ',' nums)) . splitBy ' ') (lines input)