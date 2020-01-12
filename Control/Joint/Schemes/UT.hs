module Control.Joint.Schemes.UT (UT (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Composition (Composition (Primary, run))
import Control.Joint.Abilities.Liftable (Liftable (lift))

newtype UT t u a = UT (u :. t := a)

instance Composition (UT t u) where
	type Primary (UT t u) a = u :. t := a
	run (UT x) = x

instance Liftable t u => Liftable t (UT t u) where
	lift = lift
