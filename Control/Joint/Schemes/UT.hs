module Control.Joint.Schemes.UT where

import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Class (ComonadTrans (lower))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))

newtype UT t u a = UT (u :. t := a)

instance Interpreted (UT t u) where
	type Primary (UT t u) a = u :. t := a
	run (UT x) = x

instance Monad t => MonadTrans (UT t) where
	lift x = UT $ return <$> x

instance Comonad t => ComonadTrans (UT t) where
	lower (UT x) = extract <$> x
