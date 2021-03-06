
main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)

-- The below are for exercise 4
--main = print (possibleExpressionsOfExample)
--main = print (successfulExpressionsOfExample)

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = "(" ++ (show l) ++ (show o) ++ (show r) ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = (values l) ++ (values r)

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

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

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- checks if 1. the values of the expression is allowed AND whether it evaluates correctly
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && (eval e) == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Mul, Sub, Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns,
       e <- exprs ns',
       eval e == [n]]

-- exploiting the fact that the generation of every possible calculation contains invalid expressions

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    rx <- results rs,
                    res <- combine' lx rx]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]


example = [1, 3, 7, 10, 25, 50]

possibleExpressionsOfExample = length [e | cs <- choices example,
                                           e <- exprs cs]

successfulExpressionsOfExample = length [e | cs <- choices example,
                                             e <- exprs cs, eval e /= []]
