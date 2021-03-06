module Control.Joint.Effects.Reader where

import "distributive" Data.Distributive (Distributive (distribute))

import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Schemes (TU (TU), type (<:.>))

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
	fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
	pure = Reader . const
	Reader f <*> Reader g = Reader $ \e -> f e (g e)

instance Monad (Reader e) where
	Reader g >>= f = Reader $ \e -> run (f (g e)) e

instance Distributive (Reader e) where
	distribute x = Reader $ \e -> ($ e) . run <$> x

instance Interpreted (Reader e) where
	type Primary (Reader e) a = (->) e a
	run (Reader x) = x

type instance Schema (Reader e) = TU ((->) e)

instance Transformer (Reader e) where
	build x = T. TU $ pure <$> run x
	unite = T . TU

type Configured e = Adaptable (Reader e)

get :: Configured e t => t e
get = adapt $ Reader id
