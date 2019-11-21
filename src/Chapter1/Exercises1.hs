module Chapter1.Exercises1 where

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

qsortRev :: Ord a => [a] -> [a]
qsortRev [] = []
qsortRev (x:xs) = qsortRev larger ++ [x] ++ qsortRev smaller
  where
    -- larger = [a | a <- xs, a > x]
    -- smaller = [a | a <- xs, a <= x]
    larger = filter (\a -> a > x) xs
    smaller = filter (\a -> a <= x) xs

