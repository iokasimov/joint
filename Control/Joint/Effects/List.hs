module Control.Joint.Effects.List where

import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Schemes (UT (UT), type (<.:>))

instance Interpreted [] where
	type Primary [] a = [a]
	run x = x

type instance Schema [] = UT []

instance Transformer [] where
	build x = T . UT . pure $ x
	unite = T . UT
