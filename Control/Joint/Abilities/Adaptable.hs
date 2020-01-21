module Control.Joint.Abilities.Adaptable where

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Abilities.Transformer ((:>) (T))

class Adaptable (subeff :: * -> *) (eff :: * -> *) | subeff -> eff where
	adapt :: subeff ~> eff
