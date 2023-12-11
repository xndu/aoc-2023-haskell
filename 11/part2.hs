import Data.List
main = do
    input <- readFile "input.txt"
    let cmapi f x = concat $ zipWith f x [0..]
    let diffs l = zipWith (-) (tail l) l
    let adj l = sum $ zipWith (*) [i * (length l - i) | i <- [1..]] $ map (\d -> (if d>1 then 1000000 else 1)*(d-1)+1) $ diffs $ sort l
    print $ sum $ map adj $ transpose $ cmapi (\l i -> cmapi (\e j -> [[i,j] | e=='#']) l) $ lines input