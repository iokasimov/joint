module Control.Joint.Effects.Reader where

import Control.Joint.Abilities (Interpreted (Primary, run)
	, Transformer (Schema, embed, build, unite), (:>) (T), Liftable)
import Control.Joint.Schemes (TU (TU))

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
	fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
	pure = Reader . const
	Reader f <*> Reader g = Reader $ \e -> f e (g e)

instance Monad (Reader e) where
	Reader g >>= f = Reader $ \e -> run (f (g e)) e

instance Interpreted (Reader e) where
	type Primary (Reader e) a = (->) e a
	run (Reader x) = x

instance Transformer (Reader e) where
	type Schema (Reader e) u = TU ((->) e) u
	embed x = T . TU . const $ x
	build x = T. TU $ pure <$> run x
	unite = T . TU

instance Functor u => Functor (TU ((->) e) u) where
	fmap f (TU x) = TU $ \r -> f <$> x r

instance Applicative u => Applicative (TU ((->) e) u) where
	pure = TU . pure . pure
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance (Applicative u, Monad u) => Monad (TU ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

get :: Reader e e
get = Reader $ \e -> e

type Configured e = Liftable (Reader e)
