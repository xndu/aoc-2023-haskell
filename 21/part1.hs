import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (unfoldr)

zipWt f (a,b) (c,d) = (f a c, f b d)
main = do
    input <- readFile "input.txt"
    let garden = M.fromList $ concat $ zipWith (\i -> zipWith (\j -> ((i,j),)) [0..]) [0..] $ lines input
    let sp = S.fromList $ M.keys $ M.filter (/='#') garden
    let nb p = S.map (zipWt (+) p) $ S.fromList [(0,1),(1,0),(-1,0),(0,-1)]
    let inb = S.intersection sp . nb
    let dl = iterate (S.fold S.union S.empty . S.map inb) $ S.fromList $ M.keys $ M.filter (=='S') garden
    print $ length $ dl!!64