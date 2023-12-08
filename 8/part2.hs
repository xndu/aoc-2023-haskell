import Data.List
import Data.Char (isAlpha)
import Data.Map.Strict (fromList, keys, (!))
main = do
    input <- readFile "input.txt"
    let splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
    let instr:_:body = lines input
    let dirs = map (splitEvery 3 . filter isAlpha) body
    let mp = fromList $ map head dirs `zip` [0..]
    let fl a b = dirs!!(mp!a)!!if b=='L' then 1 else 2
    print $ foldl lcm 1 $ map (length . takeWhile ((/='Z') . last) . \a -> scanl fl a $ cycle instr) $ filter ((=='A') . last) $ keys mp