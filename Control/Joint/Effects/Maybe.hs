module Control.Joint.Effects.Maybe where

import Control.Joint.Operators ((<$$>), (<**>))
import Control.Joint.Abilities.Completable (Completable (complete))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Schemes (UT (UT), type (<.:>))

instance Interpreted Maybe where
	type Primary Maybe a = Maybe a
	run x = x

type instance Schema Maybe = UT Maybe

instance Transformer Maybe where
	build x = T . UT . pure $ x
	unite = T . UT

instance Completable (Either e) Maybe where
	complete = either (const Nothing) Just

type Optional = Adaptable Maybe

nothing :: Optional t => t a
nothing = adapt Nothing
