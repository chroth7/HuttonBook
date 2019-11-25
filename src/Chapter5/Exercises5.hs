module Chapter5.Exercises5
  ( sumSquare
  , grid
  , pyths
  , factor
  , perfects
  , scalarproduct
  ) where

sumSquare :: Int -> Int
sumSquare n = sum [ x * x | x <- [1..n]]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(mm,nn) | mm <- [0..m], nn <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [ (x,y) | (x,y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n a = [ a | _ <- [1..3]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factor :: Int -> [Int]
factor n = [ x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factor x) == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x,y) <- zip xs ys ]
