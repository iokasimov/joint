module Control.Joint.Modulator where

import Control.Joint.Transformer (Transformer (Schema))

class Transformer t => Modulator t where
	{-# MINIMAL (-<$>-) #-}
	(-<$>-) :: (u a -> v b) -> Schema t u a -> Schema t v b
