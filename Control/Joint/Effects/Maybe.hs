module Control.Joint.Effects.Maybe where

import Control.Joint.Abilities (Composition (Primary, run)
	, Transformer (Schema, embed, build, unite), Adaptable (adapt), Liftable)
import Control.Joint.Schemes (UT (UT))

instance Composition Maybe where
	type Primary Maybe a = Maybe a
	run x = x

instance Transformer Maybe where
	type Schema Maybe u = UT Maybe u
	embed x = UT $ Just <$> x
	build x = UT . pure $ x
	unite = UT

instance Functor u => Functor (UT Maybe u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT Maybe u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT Maybe u) where
	UT x >>= f = UT $ x >>= maybe (pure Nothing) (run . f)

instance Adaptable (Either e) Maybe where
	adapt = either (const Nothing) Just

type Optional = Liftable Maybe
