import Data.List

count_change :: Int -> Int
count_change amount = let coins = [1, 2, 5, 10, 20, 50, 100, 200]
                      in cc coins amount
                      where
                       cc :: [Int] -> Int -> Int
                       cc [] remain    = 0
                       cc coins remain
                         | remain < 0  = 0
                         | remain == 0 = 1
                         | otherwise   = cc coins (remain - (head coins)) + cc (tail coins) remain