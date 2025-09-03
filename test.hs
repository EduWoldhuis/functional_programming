import Debug.Trace (trace)
fac n | n == 0    = 1
      | otherwise = n * fac(n-1)


joe :: Int -> Bool
joe x = if x >= 831 then True else False

sort :: (Int -> Bool) -> Int -> Int -> Int
sort f l u | l == u    = l
           | f mid     = trace msg (sort f l mid)
           | otherwise = trace msg (sort f (mid+1) u)
         where
           msg = "l: " ++ show l ++ " u: " ++ show u ++ " mid: " ++ show mid
           mid = (l + u) `div` 2

-- calcDown :: Bool -> Double -> Double
-- calcDown b n = if b then (n * n) else (n `div` 2)

-- approxSqrt :: Double -> Double -> Double -> Double
-- approxSqrt eps x i | abs ((y * y) - x) < eps = y
--                    | (y * y - x) > 0 = approxSqrt eps x 
--
--                 where
--                   y = calcDown x

printX :: String -> Int -> String
printX str 1 = str
printX str i = str ++ printX str (i-1)

printLine :: [Int] -> String
printLine [] = "+"
printLine (x:xs) = "+" ++ (printX "-" x) ++ printLine xs
    -- str = printX ""
