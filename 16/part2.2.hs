import Data.List
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

zipWt f (a,b) (c,d) = (f a c, f b d)
main = do
    input <- readFile "input.txt"
    let cave = M.fromList $ concat $ zipWith (\i -> zipWith (\j -> ((i,j),)) [0..]) [0..] $ lines input
    let dir = [(1,0),(-1,0),(0,1),(0,-1)]
    let nexts = M.fromList $ M.toList cave >>= \(l,e) -> map (\d -> (
                (l,d),
                S.fromList $ filter ((`M.member` cave) . fst) $ map (\r -> (zipWt (+) l r, r)) $ case e of
                    '.' -> [d]
                    '\\'-> [swap d]
                    '/' -> [refl $ swap d]
                    '|' -> delete (refl d) [(1,0),(-1,0)]
                    '-' -> delete (refl d) [(0,1),(0,-1)])) dir
            where refl (x,y) = (-x,-y)
    let energized s = length $ S.map fst $ fst $ until (null . snd) (\(b,f) -> (S.union b f, foldl (\a e -> S.union a $ nexts M.! e) S.empty f S.\\ b)) (S.empty, s)
    let edges = filter ((`M.notMember` cave) . uncurry (zipWt (-))) $ M.keys cave >>= \l -> map (l,) dir
    print $ maximum $ map (energized . S.singleton) edges