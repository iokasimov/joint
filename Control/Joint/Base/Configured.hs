module Control.Joint.Base.Configured where

import Control.Joint.Composition (Composition (Primary, run))
import Control.Joint.Transformer (Transformer (Schema, embed, build, unite))
import Control.Joint.Modulator (Modulator ((-<$>-)))
import Control.Joint.Schemes.TU (TU (TU))

newtype Configured e a = Configured (e -> a)

instance Functor u => Functor (TU ((->) e) u) where
	fmap f (TU x) = TU $ \r -> f <$> x r

instance Applicative u => Applicative (TU ((->) e) u) where
	pure = TU . pure . pure
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance (Applicative u, Monad u) => Monad (TU ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

instance Composition (Configured e) where
	type Primary (Configured e) a = (->) e a
	run (Configured x) = x

instance Transformer (Configured e) where
	type Schema (Configured e) u = TU ((->) e) u
	embed x = TU . const $ x
	build x = TU $ pure <$> run x
	unite = TU

instance Modulator (Configured e) where
	f -<$>- (TU x) = TU $ f <$> x

ask :: Configured e e
ask = Configured $ \e -> e
