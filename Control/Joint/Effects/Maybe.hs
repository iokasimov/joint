module Control.Joint.Effects.Maybe where

import Control.Joint.Abilities.Composition (Composition (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, embed, build, unite))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Schemes.TU (TU (TU))
import Control.Joint.Schemes.TUT (TUT (TUT))
import Control.Joint.Schemes.UT (UT (UT))

instance Functor u => Functor (UT Maybe u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT Maybe u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT Maybe u) where
	UT x >>= f = UT $ x >>= maybe (pure Nothing) (run . f)

instance Composition Maybe where
	type Primary Maybe a = Maybe a
	run x = x

instance Transformer Maybe where
	type Schema Maybe u = UT Maybe u
	embed x = UT $ Just <$> x
	build x = UT . pure $ x
	unite = UT

instance Liftable t u => Liftable t (UT Maybe u) where
	lift = lift

instance Applicative u => Liftable Maybe (UT Maybe u) where
	lift = UT . pure

instance Liftable Maybe t => Liftable Maybe (UT t u) where
	lift = lift

instance Liftable Maybe u => Liftable Maybe (TU t u) where
	lift = lift

instance Liftable Maybe u => Liftable Maybe (TUT t u t') where
	lift = lift

type Optional = Liftable Maybe
