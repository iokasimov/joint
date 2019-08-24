module Control.Joint.Schemes.TU (TU (..)) where

import Control.Joint.Core (type (:.), type (>))

newtype TU t u a = TU (t :. u > a)
