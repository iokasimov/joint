module Control.Joint.Schemes.UT where

import "base" Control.Monad (join)
import "base" Control.Applicative (Alternative (empty, (<|>)))
import "base" Data.Traversable (for)
import "comonad" Control.Comonad (Comonad (extract))
import "comonad" Control.Comonad.Trans.Class (ComonadTrans (lower))
import "transformers" Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Operators ((<$$>), (<**>))

newtype UT t u a = UT (u :. t := a)

type (<.:>) = UT

instance Interpreted (UT t u) where
	type Primary (UT t u) a = u :. t := a
	run (UT x) = x

instance (Functor t, Functor u) => Functor (t <.:> u) where
	fmap f (UT x) = UT $ f <$$> x

instance (Applicative t, Applicative u) => Applicative (t <.:> u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ f <**> x

instance (Applicative t, Alternative u) => Alternative (t <.:> u) where
	x <|> y = UT $ run x <|> run y
	empty = UT empty

instance (Traversable t, Monad t, Applicative u, Monad u) => Monad (t <.:> u) where
	UT x >>= f = UT $ x >>= \i -> join <$> for i (run . f)


instance Monad t => MonadTrans (UT t) where
	lift x = UT $ return <$> x

instance Comonad t => ComonadTrans (UT t) where
	lower (UT x) = extract <$> x
