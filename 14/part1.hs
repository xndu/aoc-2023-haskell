import Data.List

fixed f x = let y = f x in if x == y then y else fixed f y
rollEnd "" = ""
rollEnd ('O':'.':t) = '.':rollEnd('O':t)
rollEnd (h:t) = h:rollEnd t
main = do
    input <- readFile "input.txt"
    print $ sum $ zipWith (\a b -> a * length (filter (=='O') b)) [1..] $ transpose $ map (fixed rollEnd) $ transpose $ reverse $ lines input