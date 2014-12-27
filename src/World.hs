module World where

import Zipper
import Comonad

type GetWorld a = ListZipper (ListZipper a)
newtype World a = World { getWorld :: GetWorld a }

mapWorld :: (GetWorld a -> GetWorld b) -> World a -> World b
mapWorld f = World . f . getWorld

up :: World a -> World a
up = mapWorld back

down :: World a -> World a
down = mapWorld forward

left :: World a -> World a
left = mapWorld $ fmap back

right :: World a -> World a
right = mapWorld $ fmap forward

plane :: Int -> World a -> [[a]]
plane n = map listN . listN . getWorld
    where listN = list n

instance Functor World where
    fmap = mapWorld . fmap . fmap

instance Comonad World where
    extract = extract . extract . getWorld
    duplicate = World . hor . ver
      where
        hor = fmap $ zip left right
        ver = zip up down
        zip l r a = Zip (shift l) a (shift r)
          where
            shift = tail . (flip iterate a)
