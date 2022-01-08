
-- take or don't take the first element of a list. Do recursively
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- put x on the beginning of right after the beginning. Do recursively
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = [(x:y:ys)] ++ map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


-- exercise 1

choices :: [a] -> [[a]]
choices xs = [ps | ss <- subs xs,
                   ps <- perms ss]

-- exercise 2

removeElement :: Eq a => [a] -> a -> [a]
removeElement [] e = []
removeElement (x:xs) e = if x == e then xs
                         else x:(removeElement xs e)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (c:cs) as = if asMinusRemoved == as then False
                     else isChoice cs asMinusRemoved
                        where asMinusRemoved = removeElement as c

-- Exercise 4
-- check at countdownProblem
