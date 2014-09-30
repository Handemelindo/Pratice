palinnum :: (Show a, Integral a) => a -> Maybe a
palinnum n = find (divisible dividend) palin
             where
              nMax = 10 ^ n
              mMax = (10 ^ n - 1) * (10 ^ n - 1)
              dividend = [nMax, (nMax - 1)..(quot nMax 10)]
              divisible :: (Integral a) => [a] -> a -> Bool
              divisible nums num = any (valid num) nums
              valid x y = mod x y == 0 && quot x y > 10 ^ (n - 1) && quot x y < 10 ^ n
              palinTest :: (Show a, Integral a) => a -> Bool
              palinTest num = let str = show num
                              in  str == reverse str
              palin = filter palinTest [mMax, (mMax - 1)..(10 ^ (2 * n - 2))]