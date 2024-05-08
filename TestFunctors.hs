module TestFunctors where
import Data.List (sort)

data Tree a = Empty
        | Node a (Tree a) (Tree a) deriving (Show,Functor,Ord,Eq)

data Pair a = Pair a a deriving Show

increase :: Num a => Tree a -> Tree a
increase = fmap (+1)

stringtoInt :: Tree String -> Tree Int
stringtoInt = fmap length

tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
tree2 = Node 10 (Node 20 Empty Empty) (Node 30 Empty Empty)

extract :: Ord a =>Num a => Tree a -> Ord a => Num a => Tree a -> [a]
extract t1 t2 = sort (extractHelper t1 ++ extractHelper t2)

extractHelper :: Tree a -> [a]
extractHelper Empty = []
extractHelper (Node x left right) = extractHelper left ++ [x] ++ extractHelper right

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Empty
listToTree xs =
    let (leftHalf, x:rightHalf) = splitAt (length xs `div` 2) xs
    in Node x (listToTree leftHalf) (listToTree rightHalf)

mergeTree:: Ord a => Num a => Tree a -> Ord a => Num a => Tree a -> Tree a
mergeTree x y = listToTree (extract x y) 

treeS1 = Node "hola" (Node "soy" Empty Empty) (Node "sebas" Empty Empty)
treeS2 = Node "hola" (Node "soy" Empty Empty) (Node "alex" Empty Empty)

mergeStringTree :: Tree String -> Tree String -> Tree String
mergeStringTree Empty t = t
mergeStringTree t Empty = t
mergeStringTree (Node x l1 r1) (Node y l2 r2) = Node (x ++ y) (mergeStringTree l1 l2) (mergeStringTree r1 r2)
