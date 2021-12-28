

-- Luhn Algorithm

luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2


luhn4 :: Int -> Int -> Int -> Int -> Bool
luhn4 x y z w = ((luhnDouble x) + y + (luhnDouble z) + w) `mod` 10 == 0


-- general Luhn for list comprehensions

-- 

luhnDoubleOrId index x = if index `mod` 2 == 0 then x else luhnDouble x

luhn :: [Int] -> Bool
luhn xs = ((sum [luhnDoubleOrId index x | (index, x) <- zip [0..] (reverse xs)]) `mod` 10 ) == 0


