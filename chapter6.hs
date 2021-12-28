
factorial :: Int -> Int
factorial n | n <= 0 = 1
            | n > 0 = n * factorial (n-1)


sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


ex :: Int -> Int -> Int
ex n 0 = 1
ex n k = n * (ex n (k-1))


-- exercice 6.
