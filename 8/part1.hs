import Data.List
import Data.Char (isAlpha)
import Data.Map (fromList, (!))
main = do
    input <- readFile "input.txt"
    let splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
    let instr:_:body = lines input
    let dirs = map (splitEvery 3 . filter isAlpha) body
    let mp = fromList $ map head dirs `zip` [0..]
    let fl a b = dirs!!(mp!a)!!if b=='L' then 1 else 2
    print $ length $ takeWhile (/="ZZZ") $ scanl fl "AAA" $ cycle instr