module Control.Joint.Abilities.Transformer where

import Control.Applicative (Alternative (empty, (<|>)))

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))

type family Schema (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

class Interpreted t => Transformer t where
	{-# MINIMAL build, unite #-}
	build :: Applicative u => t ~> t :> u
	unite :: Primary (Schema t u) a -> (t :> u) a

infixr 3 :>
newtype (:>) t u a = T { trans :: Transformer t => Schema t u a }

instance Functor (Schema t u) => Functor (t :> u) where
	fmap f (T x) = T $ f <$> x

instance (Transformer t, Applicative (Schema t u)) => Applicative (t :> u) where
	pure = T . pure
	T f <*> T x = T $ f <*> x

instance (Transformer t, Alternative (Schema t u)) => Alternative (t :> u) where
	empty = T empty
	T f <|> T x = T $ f <|> x

instance (Transformer t, Monad (Schema t u)) => Monad (t :> u) where
	T x >>= f = T $ x >>= trans . f

instance (Interpreted (Schema t u), Transformer t) => Interpreted (t :> u) where
	type Primary (t :> u) a = Primary (Schema t u) a
	run (T x) = run x
