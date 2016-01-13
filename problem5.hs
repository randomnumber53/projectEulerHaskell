import Data.List

smallestDivUpTo :: (Integral a) => a -> a
smallestDivUpTo n =
    let factorList = concatMap primeFactorsMult $ [1..n]
        pows = groupBy (\x y -> fst x == fst y) $ sort factorList
        neededPows = map (head . reverse) pows
        result = foldl1 (*) $ map (\x -> (fst x)^(snd x)) neededPows
    in result


primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n =
    let divisor = head [x | x <- [2..], (n `mod` x) == 0]
    in divisor : (primeFactors $ quot n divisor)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack all@(x:xs) = (takeWhile (== x) all) : pack (dropWhile (== x) xs)

encode :: (Eq a) => [a] -> [(a, Int)]
encode xs = map (\xs -> (head xs, length xs)) $ pack xs

primeFactorsMult :: (Integral a) => a -> [(a, Int)]
primeFactorsMult = encode . primeFactors