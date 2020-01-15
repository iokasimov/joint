module Control.Joint.Core where

infixr 2 :.
infixr 1 :=
infixr 0 ~>

-- | Functor composition
type (:.) t u a = t (u a)

-- | Functor's object
type (:=) t a = t a

-- | Natural transformation
type (~>) t u = forall a . t a -> u a
