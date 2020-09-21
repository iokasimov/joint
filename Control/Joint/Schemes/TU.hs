module Control.Joint.Schemes.TU (TU (..)) where

import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Class (ComonadTrans (lower))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities (Interpreted (Primary, run))

newtype TU t u a = TU (t :. u := a)

instance Interpreted (TU t u) where
	type Primary (TU t u) a = t :. u := a
	run (TU x) = x

instance Monad t => MonadTrans (TU t) where
	lift = TU . return

instance Comonad t => ComonadTrans (TU t) where
	lower (TU x) = extract x
