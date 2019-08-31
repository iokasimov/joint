module Control.Joint.Schemes.TUT (TUT (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Composition (Composition (Primary, unwrap))

newtype TUT t u t' a = TUT (t :. u :. t' := a)

instance Composition (TUT t u t') where
	type Primary (TUT t u t') a = t :. u :. t' := a
	unwrap (TUT x) = x
