module Comonad where

class Functor w => Comonad w where
    -- extract . fmap f = f . extract
    extract :: w a -> a

    -- fmap (fmap f) . duplicate = duplicate . fmap f
    duplicate :: w a -> w (w a)
    duplicate = extend id

    -- extend f = fmap f . duplicate
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate

(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend
