module Control.Joint.Abilities.Adaptable where

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Interpreted (Interpreted (run))
import Control.Joint.Abilities.Transformer (Transformer (build, embed), Schema, (:>) (T, trans))

class Adaptable (eff :: * -> *) (schema :: * -> *) where
	{-# MINIMAL adapt #-}
	adapt :: eff ~> schema

type Embedding t u = (Transformer t, Functor u)
type Building t u = (Transformer t, Applicative u)

instance Adaptable t t where
	adapt = id

instance Embedding t u => Adaptable u (t :> u) where
	adapt = embed

instance Building t u => Adaptable t (t :> u) where
	adapt = build

instance
	( Embedding t (Schema u v)
	, Building u v
	) => Adaptable u (t :> u :> v) where
	adapt = embed . build

instance
	( Embedding t (Schema u v)
	, Embedding u v
	) => Adaptable v (t :> u :> v) where
	adapt = embed . embed

instance
	( Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, Building v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = embed . embed . build

instance
	( Embedding t (Schema u v)
	, Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, Embedding v w
	) => Adaptable w (t :> u :> v :> w) where
	adapt = embed . embed . embed

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Embedding w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = embed . embed . embed . embed

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Building w x
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Embedding x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Building x y
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Embedding y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Building y z
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = embed . embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Embedding z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = embed . embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Building z f
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = embed . embed . embed . embed . embed . embed . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Embedding f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = embed . embed . embed . embed . embed . embed . embed . embed

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Building f h
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = embed . embed . embed . embed . embed . embed . embed . build
