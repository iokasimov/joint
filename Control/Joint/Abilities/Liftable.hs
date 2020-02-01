module Control.Joint.Abilities.Liftable where

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Interpreted (Interpreted (run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, build, embed), (:>) (T, trans))

class Liftable (eff :: * -> *) (schema :: * -> *) where
	{-# MINIMAL lift #-}
	lift :: eff ~> schema

type Embedding t u = (Transformer t, Functor u)
type Building t u = (Transformer t, Applicative u)

instance Embedding t u => Liftable u (t :> u) where
	lift = embed

instance Building t u => Liftable t (t :> u) where
	lift = build

instance
	( Embedding t (Schema u v)
	, Building u v
	) => Liftable u (t :> u :> v) where
	lift = embed . build

instance
	( Embedding t (Schema u v)
	, Embedding u v
	) => Liftable v (t :> u :> v) where
	lift = embed . embed

instance
	( Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, Building v w
	) => Liftable v (t :> u :> v :> w) where
	lift = embed . embed . build

instance
	( Embedding t (Schema u v)
	, Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, Embedding v w
	) => Liftable w (t :> u :> v :> w) where
	lift = embed . embed . embed

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Embedding w x
	) => Liftable x (t :> u :> v :> w :> x) where
	lift = embed . embed . embed . embed

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Building w x
	) => Liftable w (t :> u :> v :> w :> x) where
	lift = embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Embedding x y
	) => Liftable y (t :> u :> v :> w :> x :> y) where
	lift = embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Building x y
	) => Liftable x (t :> u :> v :> w :> x :> y) where
	lift = embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Embedding y z
	) => Liftable z (t :> u :> v :> w :> x :> y :> z) where
	lift = embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Building y z
	) => Liftable y (t :> u :> v :> w :> x :> y :> z) where
	lift = embed . embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Embedding z f
	) => Liftable f (t :> u :> v :> w :> x :> y :> z :> f) where
	lift = embed . embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Building z f
	) => Liftable z (t :> u :> v :> w :> x :> y :> z :> f) where
	lift = embed . embed . embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Embedding f h
	) => Liftable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	lift = embed . embed . embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Building f h
	) => Liftable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	lift = embed . embed . embed . embed . embed . embed . embed . build
