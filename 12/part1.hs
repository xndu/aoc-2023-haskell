import Data.List
import Data.Maybe (listToMaybe)

ck w [] = if any (elem '#') w then 0 else 1
ck [] _ = 0
ck (d:iv) (l:en) = sum [ck (drop (n+1) d:iv) en | n <- [l..length d], '#' `notElem` take (n-l) d, Just '#' /= listToMaybe (drop n d)] + if '#' `elem` d then 0 else ck iv (l:en)
main = do
    input <- readFile "input.txt"
    let splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
    let b = map ((\[line, nums] -> (splitBy '.' line, map read $ splitBy ',' nums)) . splitBy ' ') $ lines input
    print $ sum $ map (uncurry ck) b