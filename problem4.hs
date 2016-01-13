ans4 = maximum $ map (\x -> read x :: Int) $
        filter isPalindrome . map show $
        [a*b | a <- [100..999], b <- [100..999], a <= b]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (xs == reverse xs)