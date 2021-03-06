import Prelude hiding (all, any, takeWhile, dropWhile, curry, uncurry)

-- Exercise 1

filterMap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap f p = map f . filter p

-- Exercise 2
-- This definition requires the class Eq
all :: Eq a => (a -> Bool) -> [a] -> Bool
all p vs = filter p vs == vs

-- This definition does not require the class Eq
all2 :: (a -> Bool) -> [a] -> Bool
all2 p vs = length (filter p vs) == length vs

-- This is intuitive but not lazy.
any :: Eq a => (a -> Bool) -> [a] -> Bool
any p vs = filter p vs /= []

-- Using length > 1 we can remove Eq a

--NOTE: Any requires a pass over the whole list. It could be done using lazy evaluation
anyLazy :: (a -> Bool) -> [a] -> Bool
anyLazy p [] = True
anyLazy p (x:xs) = p x || anyLazy p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = x:xs


-- Exercise 3

mapInTermsOfFoldR :: (a -> b) -> [a] -> [b]
mapInTermsOfFoldR f vs = foldr (\x -> \y -> f(x) : y) [] vs

filterInTermsOfFoldR :: (a -> Bool) -> [a] -> [a]
filterInTermsOfFoldR p vs = foldr (\x -> \y -> if p x then x : y else y) [] vs

-- Exercise 4

dec2int :: [Int] -> Int
dec2int = foldl (\x -> \y -> 10 * x + y) 0

-- Exercise 5

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x -> (\y -> f (x, y))

--test curry
--addCustom1 :: (Int, Int) -> Int
--addCustom1 (x, y) = x + y

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

--test uncurry
--addCustom2 :: Int -> Int -> Int
--addCustom2 x y = x + y

-- Exercise 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

-- int2bin = unfold (==0) (`mod` 2) (`div` 2)

type Bit = Int

chop8InTermsOfUnfold :: [Int] -> [[Int]]
chop8InTermsOfUnfold = unfold (\x -> length x == 0) (take 8) (drop 8)

mapInTermsOfUnfold :: (a -> b) -> ([a] -> [b])
mapInTermsOfUnfold f = unfold (\x -> length x == 0) (f . head) (tail)

iterateInTermsOfUnfold :: (a -> a) -> a -> [a]
iterateInTermsOfUnfold f = unfold (\x -> False) (\x -> x) (f)

-- Exercise 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [if index `mod` 2 == 0 then f x else g x | (index, x) <- zip [0..] xs]