import Data.List
main = do
    input <- readFile "input.txt"
    let mapi f x = zipWith f x [0..]
    let cmapi f x = concat $ mapi f x
    let fixed f x = let y = f x in if x == y then y else fixed f y
    let ln = lines input
    let cnx = mapi (\l i -> mapi (\e j -> case e of
            '|' -> [(i-1, j), (i+1, j)]
            '-' -> [(i, j-1), (i, j+1)]
            'L' -> [(i-1, j), (i, j+1)]
            'J' -> [(i-1, j), (i, j-1)]
            '7' -> [(i, j-1), (i+1, j)]
            'F' -> [(i, j+1), (i+1, j)]
            _ -> []) l) ln
    let start = (\[h] -> h) $ cmapi (\l i -> cmapi (\e j -> ([(i, j) | e=='S'])) l) ln
    let [m,n] = cmapi (\l i -> cmapi (\e j -> ([(i,j) | start `elem` e])) l) cnx
    let ch = [m, start, n]
    let ext = fixed (\chn -> let (i,j) = head chn in filter (not . (`elem` chn)) (cnx!!i!!j) ++ chn) ch
    print $ length ext `div` 2