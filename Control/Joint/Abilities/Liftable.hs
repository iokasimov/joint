module Control.Joint.Abilities.Liftable where

import Control.Joint.Abilities.Transformer (Transformer (Schema, build, embed), (:>) (T))

class Liftable (eff :: * -> *) (schema :: * -> *) where
	lift :: eff a -> schema a

instance (Functor u, Transformer t) => Liftable u (t :> u) where
	lift = T . embed

instance (Applicative u, Transformer t) => Liftable t (t :> u) where
	lift = T . build

instance (Functor (Schema u v), Applicative v, Transformer u, Transformer t)
	=> Liftable u (t :> u :> v) where
		lift = T . embed . T . build

instance (Functor (Schema u v), Functor v, Transformer u, Transformer t)
	=> Liftable v (t :> u :> v) where
		lift = T . embed . T . embed

instance (Functor (Schema u v), Functor (Schema v w), Functor (Schema u (v :> w))
	, Applicative v, Applicative w, Transformer u, Transformer t, Transformer v)
		=> Liftable v (t :> u :> v :> w) where
			lift = T . embed . T . embed . T . build

instance (Functor (Schema u v), Functor (Schema v w), Functor (Schema u (v :> w))
	, Applicative v, Functor w, Transformer u, Transformer t, Transformer v)
		=> Liftable w (t :> u :> v :> w) where
			lift = T . embed . T . embed . T . embed
