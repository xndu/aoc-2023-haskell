import Data.List
main = do
    input <- readFile "input.txt"
    let numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ++ map (:[]) ['0'..'9']
    let firstnum nums x = case findIndex (`isPrefixOf` x) nums of   Just i  -> i `mod` 10
                                                                    Nothing -> firstnum nums $ tail x
    print $ sum $ map (\x -> 10 * firstnum numbers x + firstnum (map reverse numbers) (reverse x)) $ lines input