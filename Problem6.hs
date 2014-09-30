sumSquareDifference :: Integer -> Integer
sumSquareDifference n = let nats = [1..n]
                        in squSum nats - sumSqu nats
                        where
                         sumSqu :: [Integer] -> Integer
                         sumSqu = sum . map (^ 2)
                         squSum :: [Integer] -> Integer
                         squSum = (^ 2) . sum