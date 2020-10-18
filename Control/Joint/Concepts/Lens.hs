module Control.Joint.Concepts.Lens where

import "comonad" Control.Comonad (extract)

import Control.Joint.Abilities.Adaptable (adapt)
import Control.Joint.Effects.Store (Store, pos, peek, retrofit)

type Lens s t = s -> Store t s

view :: Lens s t -> s -> t
view lens = pos . lens

set :: Lens s t -> t -> s -> s
set lens new = peek new . lens

over :: Lens s t -> (t -> t) -> s -> s
over lens f = extract . retrofit f . lens
