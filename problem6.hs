square x = x * x

sumOfSquares ls = sum $ map square ls

squareOfSums ls = square $ sum ls

diff n = squareOfSums [1..n] - sumOfSquares [1..n]

ans = diff 100