module Zipper where

import Comonad

data ListZipper a = Zip [a] a [a]

back :: ListZipper a -> ListZipper a
back (Zip (l:ls) a rs) = Zip ls l (a:rs)

forward :: ListZipper a -> ListZipper a
forward (Zip ls a (r:rs)) = Zip (a:ls) r rs

list :: ListZipper a -> Int -> [a]
list (Zip ls a rs) n = reverse (take n ls) ++ a:(take n rs)

instance Functor ListZipper where
    fmap f (Zip ls a rs) = Zip (fmap f ls) (f a) (fmap f rs)

instance Comonad ListZipper where
    extract (Zip _ a _) = a
    duplicate a = Zip (shift back) a (shift forward)
        where shift move = tail $ iterate move a
