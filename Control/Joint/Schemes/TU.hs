module Control.Joint.Schemes.TU (TU (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities (Interpreted (Primary, run))

newtype TU t u a = TU (t :. u := a)

instance Interpreted (TU t u) where
	type Primary (TU t u) a = t :. u := a
	run (TU x) = x
