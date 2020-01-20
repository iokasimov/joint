module Control.Joint.Effects.Reader where

import Control.Joint.Abilities (Composition (Primary, run)
	, Transformer (Schema, embed, build, unite), Liftable)
import Control.Joint.Abilities.Modulator (Modulator ((-<$>-)))
import Control.Joint.Schemes (TU (TU))

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
	fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
	pure = Reader . const
	Reader f <*> Reader g = Reader $ \e -> f e (g e)

instance Monad (Reader e) where
	Reader g >>= f = Reader $ \e -> run (f (g e)) e

instance Composition (Reader e) where
	type Primary (Reader e) a = (->) e a
	run (Reader x) = x

instance Transformer (Reader e) where
	type Schema (Reader e) u = TU ((->) e) u
	embed x = TU . const $ x
	build x = TU $ pure <$> run x
	unite = TU

instance Functor u => Functor (TU ((->) e) u) where
	fmap f (TU x) = TU $ \r -> f <$> x r

instance Applicative u => Applicative (TU ((->) e) u) where
	pure = TU . pure . pure
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance (Applicative u, Monad u) => Monad (TU ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

instance Modulator (Reader e) where
	f -<$>- (TU x) = TU $ f <$> x

get :: Reader e e
get = Reader $ \e -> e

type Configured e = Liftable (Reader e)
