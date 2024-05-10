module List where
import Control.Applicative (liftA2, Alternative, (<|>))
import GHC.Base (Alternative(empty))

data Mylist a = Empty
            | Const a (Mylist a)
            deriving (Show, Eq,Ord,Functor)

-- Example double function Int functions are Functors
foo = fmap (+2) (+3)

-- Example double function String functions are Functors
fee = fmap removeSpaces removeLineJump

-- fmap (getPostTitle) (findPost 1) -> getPostTitle <$> (findPost 1)

removeSpaces :: String -> String
removeSpaces = filter (`notElem` " ")

removeLineJump :: String -> String
removeLineJump = filter (`notElem` "\n")

--Aplicative <*>
-- Just 5
apl = Just (+3) <*> Just 2

-- [2,4,6,4,5,6]
aplFunc = [(*2), (+3)] <*> [1, 2, 3]

-- Just 150
aplicat = (*) <$> Just 5 <*> ((*) <$> Just 3 <*> Just 10)

-- Just 17
aplicatS = (+2) <$> ((*) <$> Just 5 <*> Just 3)

-- Just 15
exampleLift = liftA2 (*) (Just 5) (Just 3)
{-
data Tree a  = Empty 
            | Node a (Tree a) (Tree a)
            deriving (Show,Functor)

instance Applicative Tree where
    pure :: a -> Tree a
    pure a = Node a Empty Empty

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Node f izq1 der1) <*> (Node x izq2 der2) = Node (f x) (izq1 <*> izq2) (der1 <*> der2)
-}

instance Applicative Mylist where
  pure x = Const x Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Const f fs) <*> (Const x xs) = Const (f x) (fs <*> xs)    

instance Alternative Mylist where
    -- empty :: Mylist
    empty = Empty
    (<|>) :: Mylist a -> Mylist a -> Mylist a
    Empty <|> xs = xs
    xs <|> Empty = xs
    (Const x xs) <|> ys = Const x (xs <|> ys)

example :: Maybe Int
example = Just 1 <|> Nothing
{-
instance Applicative Tree  where
    pure :: a .> Tree
    pure a = Node a Empty Empty

    -- <*> :: Tree (a->b) -> Tree a -> Tree b
    
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing

    (Node f izq f der f)
        <*> Node x (L x) (R x)
        Node (f x) (izq f) <*> (L x)
                    (der f) <*> (R x)
-}

--instance Functor Mylist where
    -- fmap ::(a->b) -> Mylist a -> Mylist b
--    fmap _ Empty = Empty
--    fmap f (Const a xs) = Const (f a) (fmap f xs)