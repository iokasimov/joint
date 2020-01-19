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
	, Applicative v, Functor w, Transformer t, Transformer u, Transformer v)
		=> Liftable w (t :> u :> v :> w) where
	lift = T . embed . T . embed . T . embed

instance (Functor (Schema u (v :> w :> x)), Functor (Schema v (w :> x)), Functor (Schema w x)
	, Functor x, Transformer t, Transformer u, Transformer v, Transformer w)
		=> Liftable x (t :> u :> v :> w :> x) where
	lift = T . embed . T . embed . T . embed . T . embed

instance (Functor (Schema u (v :> w :> x)), Functor (Schema v (w :> x)), Functor (Schema w x)
	, Applicative x, Transformer t, Transformer u, Transformer v, Transformer w)
		=> Liftable w (t :> u :> v :> w :> x) where
	lift = T . embed . T . embed . T . embed . T . build

instance (Functor (Schema u (v :> w :> x)), Functor (Schema v (w :> x)), Functor (Schema w x), Functor (Schema x y)
	, Functor (Schema w (x :> y)), Functor (Schema v (w :> x :> y)), Functor (Schema u (v :> w :> x :> y))
		, Functor y, Transformer t, Transformer u, Transformer v, Transformer w, Transformer x)
			=> Liftable y (t :> u :> v :> w :> x :> y) where
	lift = T . embed . T . embed . T . embed . T . embed . T . embed

instance (Functor (Schema u (v :> w :> x)), Functor (Schema v (w :> x)), Functor (Schema w x)
	, Functor (Schema x y), Functor (Schema w (x :> y)), Functor (Schema v (w :> x :> y))
		, Functor (Schema u (v :> w :> x :> y)), Functor y
		, Applicative y, Transformer t, Transformer u, Transformer v, Transformer w, Transformer x)
			=> Liftable x (t :> u :> v :> w :> x :> y) where
	lift = T . embed . T . embed . T . embed . T . embed . T . build

instance (Functor (Schema u (v :> w :> x :> y :> z)), Functor (Schema v (w :> x :> y :> z))
	, Functor (Schema w (x :> y :> z)), Functor (Schema x (y :> z)), Functor (Schema y z)
		, Functor z, Transformer t, Transformer u, Transformer v
			, Transformer w, Transformer x, Transformer z, Transformer y)
				=> Liftable z (t :> u :> v :> w :> x :> y :> z) where
	lift = T . embed . T . embed . T . embed . T . embed . T . embed . T . embed

instance (Functor (Schema u (v :> w :> x :> y :> z)), Functor (Schema v (w :> x :> y :> z))
	, Functor (Schema w (x :> y :> z)), Functor (Schema x (y :> z)), Functor (Schema y z)
		, Applicative z, Transformer t, Transformer u, Transformer v
			, Transformer w, Transformer x, Transformer z, Transformer y)
				=> Liftable y (t :> u :> v :> w :> x :> y :> z) where
	lift = T . embed . T . embed . T . embed . T . embed . T . embed . T . build
