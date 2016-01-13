ans2 = sum . filter (even) . takeWhile (<= 4000000) $ fibs

fibs = 1 : 2 : next fibs
    where next (a : t@(b:_)) = (a+b) : next t

