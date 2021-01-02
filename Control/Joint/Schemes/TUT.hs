module Control.Joint.Schemes.TUT where

import "adjunctions" Data.Functor.Adjunction (Adjunction (leftAdjunct, rightAdjunct))
import "base" Control.Applicative (Alternative (empty, (<|>)))
import "comonad" Control.Comonad (Comonad (extract, extend), (=>>))
import "distributive" Data.Distributive (Distributive (collect))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Operators ((<$$$>), (<***>))

newtype TUT t t' u a = TUT (t :. u :. t' := a)

type (<:<.>:>) = TUT

instance Interpreted (TUT t t' u) where
	type Primary (TUT t t' u) a = t :. u :. t' := a
	run (TUT x) = x

instance (Functor t, Functor t', Functor u) => Functor (t <:<.>:> t' := u) where
	fmap f (TUT x) = TUT $ f <$$$> x

instance (Adjunction t' t, Monad u) => Applicative (t <:<.>:> t' := u) where
	pure = TUT . (leftAdjunct pure)
	f <*> x = TUT $ (>>= (rightAdjunct (<$$$> run x))) <$> run f

instance (Adjunction t' t, Applicative t, Functor t', Monad u, Alternative u) => Alternative (t <:<.>:> t' := u) where
	x <|> y = TUT $ (<|>) <$> run x <*> run y
	empty = TUT $ pure empty

instance (Adjunction t' t, Monad u) => Monad (t <:<.>:> t' := u) where
	x >>= f = TUT $ (>>= rightAdjunct (run . f)) <$> run x

instance (Adjunction t' t, Comonad u) => Comonad (t' <:<.>:> t := u) where
	extend f x = TUT $ (=>> leftAdjunct (f . TUT)) <$> run x
	extract = rightAdjunct extract . run

instance (Adjunction t' t, Distributive t) => MonadTrans (TUT t t') where
	lift = TUT . collect (leftAdjunct id)
