import Data.List
import qualified Data.Map as M
import Data.Bifunctor
fixed f x = let y = f x in if x == y then y else fixed f y
splitBy delim = unfoldr (\e -> if null e then Nothing else Just $ let (a,b) = break (== delim) e in (a, drop 1 b))
mapt f (a,b) = (f a, f b)
zipWt f (a,b) (c,d) = (f a c, f b d)
main = do
    input <- readFile "input.txt"
    let (start, op, p) = (M.map fst itm, M.map snd itm, parsed)
            where   parsed = map (second (map tail . splitBy ',' . drop 3) . break (==' ')) $ lines input
                    itm = M.fromList $ map (\(t:id, dests) -> (
                        if t=='b' then "<>" else id,
                        case t of
                            '%' -> (
                                M.singleton "" False,
                                \nodes (from, value) ->
                                    let state = nodes M.! id in
                                    if value then (nodes, [])
                                    else (M.adjust (M.map not) id nodes, map (,(id, not $ state M.! "")) dests))
                            '&' -> (
                                M.fromList $ filter (not . snd) $ map (bimap tail $ notElem id) parsed,
                                \nodes (from, value) ->
                                    let state = nodes M.! id in
                                    let newState = M.insert from value state in
                                    (M.insert id newState nodes, map (,(id, not $ and newState)) dests))
                            _   -> (
                                M.empty,
                                \nodes (_, value) -> (nodes, map (,(t:id,value)) dests))
                        )) parsed
    let end = foldl (\(count, state) init ->
            let result = iterate (\(state, (to, pulse):queue) ->
                    case op M.!? to of
                        Just fn -> second (queue++) $ fn state pulse
                        Nothing -> (state, queue)) (state, [init]) in
            let (n, (newState,_):_) = break (null . snd) result in
            (zipWt ((+) . length) (partition (snd .  snd . head . snd) n) count, newState)) ((0,0), start) $ replicate 1000 ("<>", ("><", False))
    print end
    print $ uncurry (*) $ fst end