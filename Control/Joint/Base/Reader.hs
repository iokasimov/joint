module Control.Joint.Base.Reader where

import Control.Joint.Composition (Composition (Primary, run))
import Control.Joint.Transformer (Transformer (Schema, embed, build, unite))
import Control.Joint.Schemes.TU (TU (TU))

newtype Reader e a = Reader (e -> a)

instance Functor u => Functor (TU ((->) e) u) where
	fmap f (TU x) = TU $ \r -> f <$> x r
--
instance Applicative u => Applicative (TU ((->) e) u) where
	pure = TU . pure . pure
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance (Applicative u, Monad u) => Monad (TU ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

instance Composition (Reader e) where
	type Primary (Reader e) a = (->) e a
	run (Reader x) = x

instance Transformer (Reader e) where
	type Schema (Reader e) u = TU ((->) e) u
	embed x = TU . const $ x
	build x = TU $ pure <$> run x
	unite = TU
