import Data.List
import Data.Char
import qualified Data.Map as M
splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
mapAt f index = zipWith id $ replicate index id ++ f:repeat id
main = do
    input <- readFile "input.txt"
    let hash = foldl (\c h -> 17*(c+ord h) `mod` 256) 0
    print $ sum $ M.mapWithKey (\k l -> (1+k) * sum (zipWith (*) [1..] $ map snd l)) $ foldl (\d e ->
        let (label,op:val) = span (`notElem` "-=") e in
        let by = if '=' == op
            then let new = (label, read val) in (\l -> 
                case label `elemIndex` map fst l of
                    Just i  -> mapAt (const new) i l
                    Nothing -> l ++ [new])
            else filter ((/=label) . fst) in
        M.adjust by (hash label) d) (M.fromList $ map (,[]) [0..255]) $ splitBy ',' input