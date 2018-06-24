module Data.Joint ((:::) (..)) where

import Data.Functor.Alt (Alt ((<!>)))

infixl 1 :::
infixr 2 :::.
infixr 1 :::+

data a ::: b where
	(:::.) :: (Alt f, Alt g) => f a -> g b -> f a ::: g b
	(:::+) :: (Alt f, Alt g, Alt h) => f a ::: g b -> h c -> (f a ::: g b) ::: h c
