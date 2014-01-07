

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

answer = foldl (+) 0 $ filter even $ takeWhile (<4000000) fibs
