
-- exercise 1

newtype T a b = T (a, b)
  deriving (Eq, Ord)

instance (Monoid a, Monoid b) => Monoid (T a b) where
  -- mempty :: (a, b)
  mempty = T (mempty, mempty)

  -- mappend :: (a, b) -> (a, b) -> (a, b)
  mappend (T (x1, y1)) (T(x2, y2)) = T (mappend x1 x2, mappend y1 y2)



-- exercise 3

data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving Show

instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr g v (Leaf x) = g x v
  foldr g v (Node l x r) = foldr g (foldr g (foldr g v r) (Leaf x)) l
