import Data.List
import Data.Char (digitToInt)
import qualified Data.Map as M

fixed f x = let y = f x in if x == y then y else fixed f y
aop1 = map . map
aop2 = zipWith . zipWith
main = do
    input <- readFile "input.txt"
    let costa = map (map digitToInt) $ lines input
    let fill = 9999
    let shift = map $ (fill:) . init
    let unshift = map $ (++[fill]) . tail
    let hz = fixed (\(h,z) -> (
                amins h (vs shift z++vs unshift z),
                amins z (vs (transpose . shift . transpose) h++vs (transpose . unshift . transpose) h)
            )) (start, start)
            where
                start = zipWith (\i -> zipWith (\j _ -> if i+j==0 then 0 else fill) [0..]) [0..] costa
                vs f a = take 3 $ tail $ zipWith (aop2 (+)) (scanl (aop2 (+)) (aop1 (const 0) costa) $ iterate f costa) $ iterate f a
                amins = foldl (aop2 min)
    print $ last $ last $ uncurry (aop2 min) hz