main = do
    input <- readFile "input.txt"
    print $ sum $ map ((\x -> read [head x, last x]) . filter (`elem` ['0'..'9'])) $ lines input