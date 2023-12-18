import Data.Tuple (swap)
import Numeric (readHex)
rot i l = uncurry (++) $ swap $ splitAt i l
main = do
    input <- readFile "input.txt"
    let instr = map ((\[_,_,color] -> (color!!7, fst $ head $ readHex $ take 5 $ drop 2 color)) . words) (lines input)
    let path = scanl (\(x,y) (dir,amt) -> case dir of
            '0' -> (x+amt,y)
            '3' -> (x,y+amt)
            '2' -> (x-amt,y)
            '1' -> (x,y-amt)) (0,0) instr
    let area2 = abs $ sum $ zipWith (\(a, b) (x, y) -> a*y-b*x) path $ rot 1 path
    let ext = sum $ map snd instr
    -- pick's thm
    print $ (area2 + 2 + ext) `div` 2