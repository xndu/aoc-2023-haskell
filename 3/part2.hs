import Data.List (elemIndices)
import Data.Char (isDigit)
main = do
    input <- readFile "input.txt"
    let mapi f x = zipWith f x [0..]
    let stars = concat $ mapi (\l i -> map (\j -> [i,j]) $ elemIndices '*' l) $ lines input
    let inums i l
          | null l = []
          | null pre = inums (i+1) $ tail l
          | otherwise = (pre, i) : inums (i+length pre) suf
          where (pre, suf) = span isDigit l
    let nums = concat $ mapi (\l i -> map (\(e, j) -> (e, [i, j]) ) $ inums 0 l) $ lines input
    let adjc p (s, p') = abs c <= 1 && -1 <= d && d <= length s where [c,d] = zipWith (-) p p'
    print $ sum $ map (\star ->
        let nb = filter (adjc star) nums in
            if length nb == 2 then product $ map (read . fst) nb else 0
        ) stars