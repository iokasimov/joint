module Control.Joint.Abilities.Adaptable where

import Control.Joint.Core (type (~>))

class Adaptable (subeff :: * -> *) (eff :: * -> *) where
	adapt :: subeff ~> eff
