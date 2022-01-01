
data Nat = Zero | Succ Nat
  deriving (Show)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add n m = int2nat ((nat2int n) + (nat2int m))

-- Exercise 1
mult :: Nat -> Nat -> Nat
mult n m = int2nat ((nat2int n) * (nat2int m))

-- Tree Data type
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show)

occursSearchTree :: Ord a => a -> Tree a -> Bool
occursSearchTree x (Leaf y) = x == y
occursSearchTree x (Node l y r) | x == y = True
                                | x < y = occursSearchTree x l
                                | otherwise = occursSearchTree x r

-- Exercise 2

occursSearchTreeWithOrdering :: Ord a => a -> Tree a -> Bool
occursSearchTreeWithOrdering x (Leaf y) = compare x y == EQ
occursSearchTreeWithOrdering x (Node l y r) | compare x y == EQ = True
                                            | compare x y == LT = occursSearchTreeWithOrdering x l
                                            | otherwise = occursSearchTreeWithOrdering x r

-- mock tree: tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

-- add new to prevent error with multiple declarations
data NewTree a = NewLeaf a | NewNode (NewTree a) (NewTree a)
  deriving (Show)

countLeafs :: NewTree a -> Int
countLeafs (NewLeaf x) = 1
countLeafs (NewNode l r) = (countLeafs l) + (countLeafs r)

isBalanced :: NewTree a -> Bool
isBalanced (NewLeaf x) = True
isBalanced (NewNode l r) | abs ((countLeafs l) - (countLeafs r)) > 1 = False
                         | otherwise = (isBalanced l) && (isBalanced r)

-- mock balanced and unbalanced trees
--veryBalancedTree = NewNode (NewNode (NewLeaf 1)
--                                    (NewLeaf 2))
--                           (NewNode (NewLeaf 4)
--                                    (NewLeaf 5))
--balancedTree = NewNode (NewNode (NewLeaf 1)
--                                (NewLeaf 2))
--                       (NewLeaf 1)
--unbalancedTree = NewNode (NewNode (NewNode (NewLeaf 1)
--                                           (NewLeaf 2))
--                                  (NewNode (NewLeaf 3)
--                                           (NewLeaf 4)))
--                         (NewLeaf 2)


-- Exercise 4
halve :: [a] -> ([a], [a])
halve xs = ((take k xs), (drop k xs))
            where k = (length xs) `div` 2

balance :: [a] -> NewTree a
balance [x] = NewLeaf x
balance xs = NewNode (balance l) (balance r)
             where (l, r) = halve xs:q