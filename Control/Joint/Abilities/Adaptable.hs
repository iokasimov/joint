module Control.Joint.Abilities.Adaptable where

import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Interpreted (Interpreted (run))
import Control.Joint.Abilities.Transformer (Transformer (build), Schema, (:>) (T, trans))

class Adaptable (eff :: * -> *) (schema :: * -> *) where
	{-# MINIMAL adapt #-}
	adapt :: eff ~> schema

type Embedding t u = (Transformer t, Monad u)
type Building t u = (Transformer t, Applicative u)

instance Adaptable t t where
	adapt = id

instance (Monad u, MonadTrans ((:>) t)) => Adaptable u (t :> u) where
	adapt = lift

instance Building t u => Adaptable t (t :> u) where
	adapt = build

instance
	( Embedding t (Schema u v)
	, MonadTrans ((:>) t)
	, Building u v
	) => Adaptable u (t :> u :> v) where
	adapt = lift . build

instance
	( Embedding t (Schema u v)
	, Embedding u v
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	) => Adaptable v (t :> u :> v) where
	adapt = lift . lift

instance
	( Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, Building v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lift . lift . build

instance
	( Embedding t (Schema u v)
	, Embedding t (Schema u (v :> w))
	, Embedding u (Schema v w)
	, Embedding v w
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	) => Adaptable w (t :> u :> v :> w) where
	adapt = lift . lift . lift

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Embedding w x
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . lift

instance (Embedding t (Schema u (v :> w :> x))
	, Embedding u (Schema v (w :> x))
	, Embedding v (Schema w x)
	, Building w x
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . build

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Embedding x y
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . lift

instance
	( Embedding t (Schema u (v :> w :> x :> y))
	, Embedding u (Schema v (w :> x :> y))
	, Embedding v (Schema w (x :> y))
	, Embedding w (Schema x y)
	, Building x y
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Embedding y z
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	, MonadTrans ((:>) y)
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . lift

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z))
	, Embedding u (Schema v (w :> x :> y :> z))
	, Embedding v (Schema w (x :> y :> z))
	, Embedding w (Schema x (y :> z))
	, Embedding x (Schema y z)
	, Building y z
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Embedding z f
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	, MonadTrans ((:>) y)
	, MonadTrans ((:>) z)
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . lift

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f))
	, Embedding u (Schema v (w :> x :> y :> z :> f))
	, Embedding v (Schema w (x :> y :> z :> f))
	, Embedding w (Schema x (y :> z :> f))
	, Embedding x (Schema y (z :> f))
	, Embedding y (Schema z f)
	, Building z f
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	, MonadTrans ((:>) y)
	, MonadTrans ((:>) z)
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . build

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Embedding f h
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	, MonadTrans ((:>) y)
	, MonadTrans ((:>) z)
	, MonadTrans ((:>) f)
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . lift

instance
	( Embedding t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Embedding u (Schema v (w :> x :> y :> z :> f :> h))
	, Embedding v (Schema w (x :> y :> z :> f :> h))
	, Embedding w (Schema x (y :> z :> f :> h))
	, Embedding x (Schema y (z :> f :> h))
	, Embedding y (Schema z (f :> h))
	, Embedding z (Schema f h)
	, Building f h
	, MonadTrans ((:>) t)
	, MonadTrans ((:>) u)
	, MonadTrans ((:>) v)
	, MonadTrans ((:>) w)
	, MonadTrans ((:>) x)
	, MonadTrans ((:>) y)
	, MonadTrans ((:>) z)
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . build
