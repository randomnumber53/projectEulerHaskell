ans3 = head . reverse $ primeFactors 600851475143

primeFactors :: (Integral a) => a -> [a]
primeFactors 1 = []
primeFactors n =
    let divisor = head [x | x <- [2..], (n `mod` x) == 0]
    in divisor : (primeFactors $ quot n divisor)