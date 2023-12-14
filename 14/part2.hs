import Data.List

fixed f x = let y = f x in if x == y then y else fixed f y
iteratep f x = let iteratep_h h a = case a `elemIndex` h of
                    Just i -> cycle (drop i h)
                    Nothing -> a:iteratep_h (h ++ [a]) (f a)
    in iteratep_h [] x
doNp n f x = iteratep f x !! n
rollEnd "" = ""
rollEnd ('O':'.':t) = '.':rollEnd('O':t)
rollEnd (h:t) = h:rollEnd t
spinCW = transpose . reverse
main = do
    input <- readFile "input.txt"
    print $ sum $ zipWith (\a b -> a * length (filter (=='O') b)) [1..] $ reverse $ doNp 1000000000 (doNp 4 (map (fixed rollEnd) . spinCW)) $ lines input