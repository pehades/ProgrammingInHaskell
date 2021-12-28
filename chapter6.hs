import Prelude hiding (and, concat, replicate, elem)

factorial :: Int -> Int
factorial n | n <= 0 = 1
            | n > 0 = n * factorial (n-1)


sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


ex :: Int -> Int -> Int
ex n 0 = 1
ex n k = n * (ex n (k-1))


-- exercice 6

and :: [Bool] -> Bool
and [] = True
and (True:bools) = and bools
and (False:bools) = False


concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs


replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = [x] ++ replicate (n-1) x

-- doesn't handle asking element from an empty list
-- need an exception but don't know yet how to implement
ind :: [a] -> Int -> a
ind (x:xs) 0 = x
ind (x:xs) n = ind xs (n-1)


elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) = a == x || elem a xs

-- exercise 7
-- important that the two given lists should be sorted from lower to higher
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then [x] ++ merge xs (y:ys)
                      else [y] ++ merge (x:xs) ys




