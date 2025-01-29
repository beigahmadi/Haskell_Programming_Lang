--Mahdi Beigahmadi, 301570853, mba188@sfu.ca
--CMPT383_H3
--Spring 2025

--Question #1
data List a = Empty | Cons a (List a) deriving (Eq, Ord, Show, Read)

listZip :: List a -> List b -> List (a, b)
listZip _ Empty = Empty
listZip Empty _ = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x, y) (listZip xs ys)

--Question #2
data Tree t = EmptyTree | Node t (Tree t) (Tree t) deriving (Show)

insert :: (Ord t) => t -> Tree t -> Tree t 
insert v EmptyTree  = Node v EmptyTree EmptyTree
insert v (Node a leftSubTree rightSubTree)
       | v < a = Node a (insert v leftSubTree) rightSubTree
       | v > a = Node a leftSubTree(insert v rightSubTree)
       | otherwise = Node v leftSubTree rightSubTree
  
--Question #3
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show, Read)

natPlus :: Nat -> Nat -> Nat
natPlus Zero n = n
natPlus (Succ m) n = Succ(natPlus m n)  

natMult :: Nat -> Nat -> Nat
natMult n Zero = Zero
natMult n (Succ m) = natPlus n (natMult n m)

--Question #4
instance Eq a => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    EmptyTree == EmptyTree = True
    Node x firstLeft firstRight == 
      Node y secondLeft secondRight = x == y 
      && firstLeft == secondLeft && firstRight == secondRight
    _ == _ = False

--Question #5
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
  fmap :: (a -> b) -> AssocList k a -> AssocList k b
  fmap _ ALEmpty = ALEmpty
  fmap f (ALCons x y tail) = ALCons x (f y) (fmap f tail)

