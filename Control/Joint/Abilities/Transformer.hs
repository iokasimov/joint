module Control.Joint.Abilities.Transformer (Transformer (..), type (:>)) where

import Control.Joint.Core (type (~>))
import Control.Joint.Abilities.Composition (Composition (Primary))

class Composition t => Transformer t where
	{-# MINIMAL embed, build, unite #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	embed :: Functor u => u ~> Schema t u
	build :: Applicative u => t ~> Schema t u
	unite :: Primary (Schema t u) a -> Schema t u a

infixr 1 :>
type (:>) t u a = Transformer t => Schema t u a
