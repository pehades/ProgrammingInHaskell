

-- exercise 1

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- exercise 4

newtype ZipList a = Z [a]
 deriving Show

instance Functor ZipList where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure x = Z [x | i <- [0..]]
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]
