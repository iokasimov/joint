module Control.Joint.Transformer (Transformer (..), type (:>)) where

import Control.Joint.Core (type (~>))
import Control.Joint.Composition (Composition)

class Composition t => Transformer t where
	{-# MINIMAL lay, wrap #-}
	type Schema (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u
	lay :: Functor u => u ~> Schema t u
	wrap :: Applicative u => t ~> Schema t u

infixr 1 :>
type (:>) t u a = Transformer t => Schema t u a
