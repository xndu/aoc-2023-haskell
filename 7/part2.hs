import Data.List
import Data.Maybe (fromJust, mapMaybe)
main = do
    input <- readFile "input.txt"
    let cardOrder = "J23456789TQKA"
    let handOrder = [[1,1,1,1,1], [2,1,1,1], [2,2,1], [3,1,1], [3,2], [4,1], [5]]
    let plays = map (\l -> [take 5 l, drop 6 l]) $ lines input
    let toHand = sortOn negate . map length . group . sort
    print $ sum $ zipWith (\p (h, c, b) -> read b*p) [1..] $ sort $ map (\[h, b] -> (
        let hand = toHand $ filter (/='J') h in
            snd $ last $ filter (\(hnd, i) -> length hnd >= length hand && and (zipWith (>=) hnd hand)) $ zip handOrder [0..],
        map (fromJust . (`elemIndex` cardOrder)) h,
        b)) plays