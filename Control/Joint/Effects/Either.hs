module Control.Joint.Effects.Either where

import Control.Joint.Operators ((<$$>), (<**>))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Schemes (UT (UT), type (<.:>))

instance Interpreted (Either e) where
	type Primary (Either e) a = Either e a
	run x = x

type instance Schema (Either e) = UT (Either e)

instance Transformer (Either e) where
	build x = T . UT . pure $ x
	unite = T . UT

type Failable e = Adaptable (Either e)

failure :: Failable e t => e -> t a
failure = adapt . Left
