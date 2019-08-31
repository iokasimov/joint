module Control.Joint.Base.Maybe where

import Control.Joint.Composition (Composition (Primary, unwrap))
import Control.Joint.Transformer (Transformer (Schema, lay, wrap))
import Control.Joint.Schemes.UT (UT (UT))

instance Functor u => Functor (UT Maybe u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT Maybe u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT Maybe u) where
	UT x >>= f = UT $ x >>= maybe (pure Nothing) (unwrap . f)

instance Composition Maybe where
	type Primary Maybe a = Maybe a
	unwrap x = x

instance Transformer Maybe where
	type Schema Maybe u = UT Maybe u
	lay x = UT $ Just <$> x
	wrap x = UT . pure $ x
