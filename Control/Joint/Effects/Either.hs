module Control.Joint.Effects.Either where

import Control.Joint.Abilities (Composition (Primary, run)
	, Transformer (Schema, embed, build, unite), Liftable)
import Control.Joint.Schemes (UT (UT))

instance Composition (Either e) where
	type Primary (Either e) a = Either e a
	run x = x

instance Transformer (Either e) where
	type Schema (Either e) u = UT (Either e) u
	embed x = UT $ Right <$> x
	build x = UT . pure $ x
	unite = UT

instance Functor u => Functor (UT (Either e) u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT (Either e) u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT (Either e) u) where
	UT x >>= f = UT $ x >>= either (pure . Left) (run . f)

failure :: e -> Either e a
failure = Left

type Failable e = Liftable (Either e)
