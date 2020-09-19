module Control.Joint.Abilities.Completable where

import Control.Joint.Core (type (~>))

class Completable (subeff :: * -> *) (eff :: * -> *) | subeff -> eff where
	complete :: subeff ~> eff
