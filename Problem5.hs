smallestMultiple :: (Integral a) => a -> a
smallestMultiple n = let dividend = [1..n]
                     in product $ canclation dividend
                     where
                      cancle x y = if mod x y == 0
                                     then div x y
                                     else x
                      canclation [] = []
                      canclation xs = head xs : canclation (map (flip cancle $ head xs) $ tail xs)