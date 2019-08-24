module Control.Joint.Schemes.UTU (UTU (..)) where

import Control.Joint.Core (type (:.), type (>))

newtype UTU t u a = UTU (u :. t u > a)
