
import Data.Char


a = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]

grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

square n = [(x, y) | (x, y) <- grid n n , x /= y]

replicateCustom :: Int -> a -> [a]

replicateCustom n a = [a | x <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]

pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [d | d <- [1..(n-1)], (n `mod` d) == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

b = [(x, y) | x <- [1, 2], y <- [3, 4]]


c = concat [[(1, y) | y <- [3, 4]], [(2, y) | y <- [3, 4]]]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

positionsInTermsOfFind x xs = find x (zip xs [0..])

scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]



-- Caesar cipher

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | otherwise = c


encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]






