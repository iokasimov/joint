module Control.Joint.Schemes.TUT (TUT (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))

-- TODO: think about decomposing it on UT and TU
newtype TUT t u t' a = TUT (t :. u :. t' := a)

instance Interpreted (TUT t u t') where
	type Primary (TUT t u t') a = t :. u :. t' := a
	run (TUT x) = x
