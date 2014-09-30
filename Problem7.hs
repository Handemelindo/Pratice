nthPrim :: (Integral a) => Int -> a
nthPrim n = prims [2..] !! (n - 1)
            where
             prims :: (Integral a) => [a] -> [a]
             prims xs = head xs : prims (filter ((/= 0) . flip mod (head xs)) $ tail xs)