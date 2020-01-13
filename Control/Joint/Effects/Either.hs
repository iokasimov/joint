module Control.Joint.Effects.Either where

import Control.Joint.Abilities.Composition (Composition (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, embed, build, unite))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Schemes.TU (TU (TU))
import Control.Joint.Schemes.TUT (TUT (TUT))
import Control.Joint.Schemes.UT (UT (UT))

instance Functor u => Functor (UT (Either e) u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT (Either e) u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT (Either e) u) where
	UT x >>= f = UT $ x >>= either (pure . Left) (run . f)

instance Composition (Either e) where
	type Primary (Either e) a = Either e a
	run x = x

instance Transformer (Either e) where
	type Schema (Either e) u = UT (Either e) u
	embed x = UT $ Right <$> x
	build x = UT . pure $ x
	unite = UT

instance Applicative u => Liftable (Either e) (UT (Either e) u) where
	lift = build

instance Functor u => Liftable u (UT (Either e) u) where
	lift = embed

type Failable e = Liftable (Either e)
