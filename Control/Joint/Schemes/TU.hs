module Control.Joint.Schemes.TU where

import "base" Control.Monad (join)
import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Class (ComonadTrans (lower))
import "distributive" Data.Distributive (Distributive (collect))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities (Interpreted (Primary, run))
import Control.Joint.Operators ((<$$>), (<**>))

newtype TU t u a = TU (t :. u := a)

type (<:.>) = TU

instance Interpreted (t <:.> u) where
	type Primary (TU t u) a = t :. u := a
	run (TU x) = x

instance (Functor t, Functor u) => Functor (t <:.> u) where
	fmap f (TU x) = TU $ f <$$> x

instance (Applicative t, Applicative u) => Applicative (t <:.> u) where
	pure = TU . pure . pure
	TU f <*> TU x = TU $ f <**> x

instance (Monad t, Distributive t, Monad u) => Monad (t <:.> u) where
	TU x >>= f = TU $ x >>= \i -> join <$> collect (run . f) i

instance Monad t => MonadTrans (TU t) where
	lift = TU . return

instance Comonad t => ComonadTrans (TU t) where
	lower (TU x) = extract x
