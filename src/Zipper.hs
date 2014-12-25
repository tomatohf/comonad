module Zipper where

import Comonad

data ListZipper a = Zip [a] a [a]

left :: ListZipper a -> ListZipper a
left (Zip (l:ls) a rs) = Zip ls l (a:rs)

right :: ListZipper a -> ListZipper a
right (Zip ls a (r:rs)) = Zip (a:ls) r rs

get :: ListZipper a -> a
get (Zip _ a _) = a

list :: ListZipper a -> Int -> [a]
list (Zip ls a rs) n = reverse (take n ls) ++ a:(take n rs)

instance Functor ListZipper where
    fmap f (Zip ls a rs) = Zip (fmap f ls) (f a) (fmap f rs)

instance Comonad ListZipper where
    extract = get
    duplicate a = Zip (shift left) a (shift right)
        where shift move = tail $ iterate move a
