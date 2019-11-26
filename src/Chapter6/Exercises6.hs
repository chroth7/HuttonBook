module Chapter6.Exercises6
  ( factorial
  , sumdown
  , (^^^)
  , euclid
  ) where

factorial :: Int -> Int
factorial 1 = 1
factorial n
  | n > 0     = n * factorial (n -1)
  | otherwise = 0

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n
  | n < 0     = 0
  | otherwise = n + sumdown (n - 1)

(^^^) :: Int -> Int -> Int
(^^^) a 0 = 1
(^^^) a 1 = a
(^^^) a b
  | b > 0     = a * (a ^^^ (b - 1))
  | otherwise = 0 -- cheating

euclid :: Int -> Int -> Int
euclid m n
  | m <= 0     = 0
  | n <= 0     = 0
  | m == n    = m
  | m > n     = euclid (m - n) n
  | m < n     = euclid m (n - m)


