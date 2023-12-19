import Data.List
import qualified Data.Map as M
splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
main = do
    input <- readFile "input.txt"
    let [wf, pr] = splitBy "" $ lines input
    let wkfl = M.fromList $ [("A", sum),("R", const 0)] ++ map ((\[n, rl] -> (
                n,
                let sp = splitBy ',' rl in
                    foldr (\cond fn m ->
                        let [v:c:r,val] = splitBy ':' cond in
                        if stc c (m M.! v) $ read r then val >@ m else fn m
                    ) (last sp >@) $ init sp
            )) . splitBy '{' . init) wf
            where   (>@) = (wkfl M.!)
                    stc '<' = (<)
                    stc '>' = (>)
    let parts = map (M.fromList . map (\(k:_:v) -> (k, read v)) . splitBy ',' . tail . init) pr
    print $ sum $ map (wkfl M.! "in") parts