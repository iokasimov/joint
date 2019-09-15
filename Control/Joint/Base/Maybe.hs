module Control.Joint.Base.Maybe where

import Control.Joint.Composition (Composition (Primary, run))
import Control.Joint.Transformer (Transformer (Schema, embed, build))
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
