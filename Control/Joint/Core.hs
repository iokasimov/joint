module Control.Joint.Core where

infixr 1 :.
infixr 1 .:

-- | Functor composition
type (:.) t u a = t (u a)
-- | Reversed functor composition
type (.:) t u a = u (t a)

-- | Functor object
infixr 0 >
type (>) t a = t a

-- | Natural transformation
type (~>) t u = forall a . (Functor t, Functor u) => t a -> u a
