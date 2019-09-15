module Control.Joint.Schemes.TUT (TUT (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Composition (Composition (Primary, run))

newtype TUT t u t' a = TUT (t :. u :. t' := a)

instance Composition (TUT t u t') where
	type Primary (TUT t u t') a = t :. u :. t' := a
	run (TUT x) = x
