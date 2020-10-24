module Control.Joint.Concepts.Lens where

import "comonad" Control.Comonad (extract)

import Control.Joint.Abilities.Adaptable (adapt)
import Control.Joint.Effects.State (Stateful, State (State))
import Control.Joint.Effects.Store (Store (Store), pos, peek, retrofit)

type Lens s t = s -> Store t s

view :: Lens s t -> s -> t
view lens = pos . lens

set :: Lens s t -> t -> s -> s
set lens new = peek new . lens

over :: Lens s t -> (t -> t) -> s -> s
over lens f = extract . retrofit f . lens

zoom :: Stateful bg t => Lens bg ls -> State ls a -> t a
zoom lens (State f) = adapt . State $ (\(Store (p, g)) -> (\(x,y) -> (g x, y)) . f $ p) . lens

_1 :: Lens (a, b) a
_1 (x, y) = Store (x, \x' -> (x', y))

_2 :: Lens (a, b) b
_2 (x, y) = Store (y, \y' -> (x, y'))
