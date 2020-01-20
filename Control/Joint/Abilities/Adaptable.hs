module Control.Joint.Abilities.Adaptable where

import Control.Joint.Core (type (~>))

class Adaptable (subeff :: * -> *) (eff :: * -> *) | subeff -> eff where
	{-# MINIMAL adapt #-}
	adapt :: subeff ~> eff
