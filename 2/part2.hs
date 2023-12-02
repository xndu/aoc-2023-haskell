import Data.Text (pack, unpack, splitOn, drop)
main = do
    input <- readFile "input.txt"
    let spl = splitOn . pack
    let splitline line = spl "; " line >>= spl ", "
    let to_vals = map (map unpack . spl " ") . splitline
    print $ sum $ [product $ map (\col -> foldl max 0 [read num | [num, typ] <- to_vals tries, typ == col]) ["red", "green", "blue"]
        | tries <- map (last . spl ": " . pack) $ lines input]