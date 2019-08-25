module Control.Joint.Schemes.UTU (UTU (..)) where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Composition (Composition (Primary, unwrap))

newtype UTU t u a = UTU (u :. t u := a)

instance Composition (UTU t u) where
	type Primary (UTU t u) a = u :. t u := a
	unwrap (UTU x) = x
