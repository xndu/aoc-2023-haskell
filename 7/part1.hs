import Data.List
import Data.Maybe (fromJust, mapMaybe)
main = do
    input <- readFile "input.txt"
    let cardOrder = "23456789TJQKA"
    let handOrder = [[1,1,1,1,1], [2,1,1,1], [2,2,1], [3,1,1], [3,2], [4,1], [5]]
    let plays = map (\l -> [take 5 l, drop 6 l]) $ lines input
    let toHand = sortOn negate . map length . group . sort
    print $ sum $ zipWith (\p (h, c, b) -> read b*p) [1..] $ sort $ map (\[h, b] -> (
        fromJust $ toHand h `elemIndex` handOrder,
        map (fromJust . (`elemIndex` cardOrder)) h,
        b)) plays