import Data.List (findIndices)
import Data.Char (isDigit)
main = do
    input <- readFile "input.txt"
    let mapi f x = zipWith f x [0..]
    let syms = concat $ mapi (\l i -> map (\j -> [i,j]) $ findIndices (not . (`elem` '.':['0'..'9'])) l) $ lines input
    let inums i l
          | null l = []
          | null pre = inums (i+1) $ tail l
          | otherwise = (pre, i) : inums (i+length pre) suf
          where (pre, suf) = span isDigit l
    let nums = concat $ mapi (\l i -> map (\(e, j) -> (e, [i, j]) ) $ inums 0 l) $ lines input
    let adjc p (s, p') = abs c <= 1 && -1 <= d && d <= length s where [c,d] = zipWith (-) p p'
    print $ sum $ map (read . fst) $ filter (\n -> any (`adjc` n) syms) nums