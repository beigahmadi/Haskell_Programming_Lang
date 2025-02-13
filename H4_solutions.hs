--Mahdi Beigahmadi, 301570853, mba188@sfu.ca
--CMPT383_H4
--Spring 2025

--Question 1
data ErrJst e j = Err e | Jst j deriving (Show)
instance Functor (ErrJst e) where
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap _ (Err e) = Err e
    fmap f (Jst j) = Jst (f j)

--Question 2
instance Applicative (ErrJst e) where
  pure :: a -> ErrJst e a
  (<*>) :: ErrJst e (a -> b) -> ErrJst e a -> ErrJst e b
  Err e <*> _   = Err e
  Jst a <*> r   = fmap a r
  pure = Jst

--Question 3
instance Monad(ErrJst e) where
  return :: a -> ErrJst e a
  return = pure
  (>>=) :: ErrJst e a -> (a -> ErrJst e b) -> ErrJst e b
  Err e >>= _ = Err e
  Jst a >>= f = f a

--Question 4
join :: Monad m => m (m a) -> m a
join mm = mm >>= id

--Question 5
data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)
instance Foldable LTree where
 foldMap :: Monoid m => (a -> m) -> LTree a -> m
 foldMap f (Leaf x) = f x
 foldMap f (LNode l r) = mappend (foldMap f l) (foldMap f r)