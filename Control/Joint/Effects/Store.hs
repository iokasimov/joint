module Control.Joint.Effects.Store where

import "comonad" Control.Comonad (Comonad (extract, extend))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Operators ((<$$>))

newtype Store s a = Store ((,) s :. (->) s := a)

instance Functor (Store s) where
	fmap f (Store x) = Store $ f <$$> x

instance Comonad (Store s) where
	extend f (Store (s, g)) = Store (s, \s' -> f (Store (s', g)))
	extract (Store (s, g)) = g s
