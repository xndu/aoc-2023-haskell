import Data.List
import qualified Data.Map as M
splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
main = do
    input <- readFile "input.txt"
    let wf = head $ splitBy "" $ lines input
    let wkfl = M.fromList $ [("A", product . M.map length), ("R", const 0)] ++ map ((\[n, rl] -> (
                n,
                let sp = splitBy ',' rl in
                    foldr (\cond fn m ->
                        let [v:c:r,val] = splitBy ':' cond in
                        let (p,q) = partition (stc c $ read r) $ m M.! v in
                        fn (M.insert v p m) + val >@ M.insert v q m
                    ) (last sp >@) (init sp)
            )) . splitBy '{' . init) wf
            where   (>@) = (wkfl M.!)
                    stc '<' = (<)
                    stc '>' = (>)
    print $ wkfl M.! "in" $ M.fromList $ map (,[1..4000]) "xmas"