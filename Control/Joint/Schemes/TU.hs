module Control.Joint.Schemes.TU (TU (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Composition (Composition (Primary, run))
import Control.Joint.Abilities.Liftable (Liftable (lift))

newtype TU t u a = TU (t :. u := a)

instance Composition (TU t u) where
	type Primary (TU t u) a = t :. u := a
	run (TU x) = x

instance Liftable u t => Liftable u (TU t u) where
	lift = lift
