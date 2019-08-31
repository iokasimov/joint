module Control.Joint.Base.Either where

import Control.Joint.Composition (Composition (Primary, unwrap))
import Control.Joint.Transformer (Transformer (Schema, lay, wrap))
import Control.Joint.Schemes.UT (UT (UT))

instance Functor u => Functor (UT (Either e) u) where
	fmap f (UT x) = UT $ (fmap . fmap) f x

instance Applicative u => Applicative (UT (Either e) u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ (<*>) <$> f <*> x

instance (Applicative u, Monad u) => Monad (UT (Either e) u) where
	UT x >>= f = UT $ x >>= either (pure . Left) (unwrap . f)

instance Composition (Either e) where
	type Primary (Either e) a = Either e a
	unwrap x = x

instance Transformer (Either e) where
	type Schema (Either e) u = UT (Either e) u
	lay x = UT $ Right <$> x
	wrap x = UT . pure $ x
