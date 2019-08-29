module Control.Joint.Base.Either where

import Control.Joint.Composition (Composition (Primary, unwrap))
import Control.Joint.Transformer (Transformer (Schema, lay, wrap))
import Control.Joint.Schemes.UT (UT (UT))

instance Composition (Either e) where
	type Primary (Either e) a = Either e a
	unwrap x = x

instance Transformer (Either e) where
	type Schema (Either e) u = UT (Either e) u
	lay x = UT $ Right <$> x
	wrap x = UT . pure $ x
