import Data.Tuple (swap)
rot i l = uncurry (++) $ swap $ splitAt i l
main = do
    input <- readFile "input.txt"
    let instr = map ((\[dir,amt,_] -> (dir, read amt)) . words) (lines input)
    let path = scanl (\(x,y) (dir,amt) -> case dir of
            "R" -> (x+amt,y)
            "U" -> (x,y+amt)
            "L" -> (x-amt,y)
            "D" -> (x,y-amt)) (0,0) instr
    let area2 = abs $ sum $ zipWith (\(a, b) (x, y) -> a*y-b*x) path $ rot 1 path
    let ext = sum $ map snd instr
    -- pick's thm
    print $ (area2 + 2 + ext) `div` 2