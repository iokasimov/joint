module Control.Joint.Effects.Maybe where

import Control.Joint.Operators ((<$$>), (<**>))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, embed, build, unite), (:>) (T))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Schemes (UT (UT))

instance Interpreted Maybe where
	type Primary Maybe a = Maybe a
	run x = x

instance Transformer Maybe where
	type Schema Maybe = UT Maybe
	embed x = T . UT $ Just <$> x
	build x = T . UT . pure $ x
	unite = T . UT

instance Functor u => Functor (UT Maybe u) where
	fmap f (UT x) = UT $ f <$$> x

instance Applicative u => Applicative (UT Maybe u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ f <**> x

instance (Applicative u, Monad u) => Monad (UT Maybe u) where
	UT x >>= f = UT $ x >>= maybe (pure Nothing) (run . f)

instance Adaptable (Either e) Maybe where
	adapt = either (const Nothing) Just

type Optional = Liftable Maybe

nothing :: Optional t => t a
nothing = lift Nothing
