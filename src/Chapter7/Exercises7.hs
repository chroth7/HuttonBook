module Chapter7.Exercises7
  ( filtermap
  , all'
  , any'
  , takeWhile'
  , dropWhile'
  , map'
  , filter'
  ) where

-- Replace [ f x | x <- xs, p x ]
filtermap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtermap p f = map f . filter p

-- Replace all of the following
-- all :: (a -> Bool) -> [a] -> Bool
-- any :: (a -> Bool) -> [a] -> Bool
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\v a -> p v && a) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\v a -> p v || a) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\v a -> f v : a) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr ff []
  where ff v a
          | p v       = v : a
          | otherwise = a
-- filter' p = foldr (\v a -> if p v then v : a else a) []
