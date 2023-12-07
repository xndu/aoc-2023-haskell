import Data.List (unfoldr, intersect)
main = do
    input <- readFile "input.txt"
    let splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
    print $ fst $ foldl (\(t, x) y ->
            (t + length (filter (==1) x), map (-1 +) x >>= (\i -> if i==0 then [1..y] else [i])))
        (0, [1..length (lines input)]) $ map ((\(x, y) ->
            let wins = splitEvery 3 x in
            let nums = splitEvery 3 $ drop 1 y in
            length $ intersect nums wins
        ) . break (== '|') . drop 9) $ lines input