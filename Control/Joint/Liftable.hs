module Control.Joint.Liftable where

class Liftable (eff :: * -> *) (schema :: * -> *) where
	lift :: eff a -> schema a
