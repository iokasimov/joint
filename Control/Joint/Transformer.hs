module Control.Joint.Transformer (Transformer (..), type (:>)) where

import Control.Joint.Core (type (~>))
import Control.Joint.Composition (Composition)

class Composition t => Transformer t where
	{-# MINIMAL embed, build #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	embed :: Functor u => u ~> Schema t u
	build :: Applicative u => t ~> Schema t u

infixr 1 :>
type (:>) t u a = Transformer t => Schema t u a
