module Control.Joint.Effects.Either where

import Control.Joint.Operators ((<$$>), (<**>))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, embed, build, unite), (:>) (T))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Schemes (UT (UT))

instance Interpreted (Either e) where
	type Primary (Either e) a = Either e a
	run x = x

instance Transformer (Either e) where
	type Schema (Either e) u = UT (Either e) u
	embed x = T . UT $ Right <$> x
	build x = T . UT . pure $ x
	unite = T . UT

instance Functor u => Functor (UT (Either e) u) where
	fmap f (UT x) = UT $ f <$$> x

instance Applicative u => Applicative (UT (Either e) u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ f <**> x

instance (Applicative u, Monad u) => Monad (UT (Either e) u) where
	UT x >>= f = UT $ x >>= either (pure . Left) (run . f)

type Failable e = Liftable (Either e)

failure :: Failable e t => e -> t a
failure = lift . Left
