import Data.Text (pack, unpack, splitOn)
main = do
    input <- readFile "input.txt"
    let spl = splitOn . pack
    let splitline line = spl "; " line >>= spl ", "
    let to_vals = map (map unpack . spl " ") . splitline
    print $ sum [read $ drop 5 $ unpack label
        | [label, tries] <- map (spl ": " . pack) $ lines input,
        all (\[num, typ] -> read num <= case typ of
            "red"   -> 12
            "green" -> 13
            "blue"  -> 14
            _       -> -1) $ to_vals tries]