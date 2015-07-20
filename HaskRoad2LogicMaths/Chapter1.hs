-- |

module Code.Pratice.HaskRoad2LogicMaths.Chapter1 where
  import Data.List
  -- Skip -1.9
  -- Exercise 1.9
  maxInt' :: [Int] -> Int
  maxInt' []       = error "empty list"
  maxInt' [x]      = x
  maxInt' (x : xs) = max x $ maxInt' xs

  -- Exercise 1.10
  removeFst' :: Integer -> [Integer] -> [Integer]
  removeFst' _ [] = []
  removeFst' n [x]
    | x == n     = []
    | otherwise  = [x]
  removeFst' n (x : xs)
    | n == x     = xs
    | otherwise  = x : (removeFst' n xs)

  -- Exercise 1.13
  count' :: Char -> String -> Int
  count' c []   = 0
  count' c (s : ss)
    | c == s    = count' c ss + 1
    | otherwise = count' c ss

  -- Exercise 1.14
  blowup' :: String -> String
  blowup' = concat . zipWith (\ n c -> take n $ repeat c) [1..]

  prefix' :: String -> String -> Bool
  prefix' [] _              = True
  prefix' _ []              = False
  prefix' (x : xs) (y : ys) = x == y && prefix' xs ys

  -- Exercise 1.17
  subString' :: String -> String -> Bool
  subString' [] _         = True
  subString' _ []         = False
  subString' s1 s2@(y : ys) = prefix' s1 s2 || subString' s1 ys

  -- Exercise 1.20
  lengths' :: [[a]] -> [Int]
  lengths' = map length

  -- Exercise 1.21
  sumLengths' :: [[a]] -> Int
  sumLengths' = sum . map length
