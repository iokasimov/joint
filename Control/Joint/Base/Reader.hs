module Control.Joint.Base.Reader where

import Control.Joint.Composition (Composition (Primary, unwrap))
import Control.Joint.Transformer (Transformer (Schema, lay, wrap))
import Control.Joint.Schemes.TU (TU (TU))

newtype Reader r a = Reader (r -> a)

instance Composition (Reader e) where
	type Primary (Reader e) a = (->) e a
	unwrap (Reader x) = x

instance Transformer (Reader r) where
	type Schema (Reader r) u = TU ((->) r) u
	lay x = TU . const $ x
	wrap x = TU $ pure <$> unwrap x
