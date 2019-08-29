module Control.Joint.Base.Maybe where

import Control.Joint.Composition (Composition (Primary, unwrap))
import Control.Joint.Transformer (Transformer (Schema, lay, wrap))
import Control.Joint.Schemes.UT (UT (UT))

instance Composition Maybe where
	type Primary Maybe a = Maybe a
	unwrap x = x

instance Transformer Maybe where
	type Schema Maybe u = UT Maybe u
	lay x = UT $ Just <$> x
	wrap x = UT . pure $ x
