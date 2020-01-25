module Control.Joint.Abilities.Transformer (Transformer (..), (:>) (..)) where

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Composition (Composition (Primary, run))

class Composition t => Transformer t where
	{-# MINIMAL embed, build, unite #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	embed :: Functor u => u ~> t :> u
	build :: Applicative u => t ~> t :> u
	unite :: Primary (Schema t u) a -> (t :> u) a

infixr 3 :>
newtype (:>) t u a = T { trans :: Transformer t => Schema t u a }

instance Functor (Schema t u) => Functor (t :> u) where
	fmap f (T x) = T $ f <$> x

instance (Transformer t, Applicative (Schema t u)) => Applicative (t :> u) where
	pure = T . pure
	T f <*> T x = T $ f <*> x

instance (Transformer t, Monad (Schema t u)) => Monad (t :> u) where
	T x >>= f = T $ x >>= trans . f

instance (Composition (Schema t u), Transformer t) => Composition (t :> u) where
	type Primary (t :> u) a = Primary (Schema t u) a
	run (T x) = run x
