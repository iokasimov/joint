module Control.Joint.Effects.Store where

import "comonad" Control.Comonad (Comonad (extract, extend))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Operators ((<$$>))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))

newtype Store s a = Store ((,) s :. (->) s := a)

instance Functor (Store s) where
	fmap f (Store x) = Store $ f <$$> x

instance Comonad (Store s) where
	extend f (Store (s, g)) = Store (s, \s' -> f (Store (s', g)))
	extract (Store (s, g)) = g s

instance Interpreted (Store s) where
	type Primary (Store s) a = (,) s :. (->) s := a
	run (Store x) = x

pos :: Store s a -> s
pos (Store (s, _)) = s

seek :: s -> Store s a -> Store s a
seek s (Store (_, f)) = Store (s, f)

peek :: s -> Store s a -> a
peek s (Store (_, f)) = f s

retrofit :: (s -> s) -> Store s a -> Store s a
retrofit g (Store (s, f)) = Store (g s, f)

type Lens s t = s -> Store t s

view :: Lens s t -> s -> t
view lens = pos . lens

set :: Lens s t -> t -> s -> s
set lens new = peek new . lens

over :: Lens s t -> (t -> t) -> s -> s
over lens f = extract . retrofit f . lens
