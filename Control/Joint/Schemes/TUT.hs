module Control.Joint.Schemes.TUT where

import "adjunctions" Data.Functor.Adjunction (Adjunction (leftAdjunct))
import "distributive" Data.Distributive (Distributive (collect))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))

newtype TUT t t' u a = TUT (t :. u :. t' := a)

instance Interpreted (TUT t t' u) where
	type Primary (TUT t t' u) a = t :. u :. t' := a
	run (TUT x) = x

instance (Adjunction t' t, Distributive t) => MonadTrans (TUT t t') where
	lift = TUT . collect (leftAdjunct id)
