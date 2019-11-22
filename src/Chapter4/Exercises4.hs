module Chapter4.Exercises4
  ( halve
  , third1
  , third2
  -- , third3
  , safetailCE
  , safetailGE
  , safetailPM
  , luhn
  ) where

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- third, with head/tail, bangbang, pattern matching
third1 :: [a] -> a
third1 xs
  | length xs < 3 = head xs
  | otherwise     = head $ tail $ tail xs

third2 :: [a] -> a
third2 xs
  | length xs < 3 = head xs
  | otherwise     = xs !! 2


-- third3 :: [a] -> a
-- third3 list@(_:_:x:_)
--   | length list < 3 = head list
--   | otherwise       = x

safetailCE :: [a] -> [a]
safetailCE xs = if null xs then xs else tail xs

safetailGE :: [a] -> [a]
safetailGE xs
  | null xs = []
  | otherwise = tail xs

safetailPM :: [a] -> [a]
safetailPM []     = []
safetailPM (x:xs) = xs

luhnDouble :: Int -> Int
luhnDouble x
  | 2 * x >= 10  = 2 * x - 9
  | otherwise    = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = luhnSum `mod` 10 == 0
  where luhnSum = sum [luhnDouble a,b,luhnDouble c,d]
