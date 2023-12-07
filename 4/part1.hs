import Data.List (unfoldr, intersect)
main = do
    input <- readFile "input.txt"
    let splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
    print $ sum $ map ((\(x, y) ->
            let wins::[Int] = map read $ splitEvery 3 x in
            let nums = map read $ splitEvery 3 $ drop 1 y in
            (`div` 2) $ (2 ^) $ length $ intersect nums wins
        ) . break (== '|') . drop 10) $ lines input