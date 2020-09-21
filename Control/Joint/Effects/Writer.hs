module Control.Joint.Effects.Writer where

import Control.Joint.Operators ((<$$>), (<**>))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Schemes (UT (UT))

newtype Writer e a = Writer (e, a)

instance Functor (Writer e) where
	fmap f (Writer x) = Writer $ f <$> x

instance Monoid e => Applicative (Writer e) where
	pure = Writer . (,) mempty
	f <*> v = Writer $ k (run f) (run v) where
		k ~(e, a) ~(e', b) = (e <> e', a b)

instance Monoid e => Monad (Writer e) where
	Writer (e, x) >>= f = let (e', b) = run $ f x in
		Writer (e <> e', b)

instance Interpreted (Writer e) where
	type Primary (Writer e) a = (e, a)
	run (Writer x) = x

type instance Schema (Writer e) = UT ((,) e)

instance Monoid e => Transformer (Writer e) where
	build = T . UT . pure . run
	unite = T . UT

instance Functor u => Functor (UT ((,) e) u) where
	fmap f (UT x) = UT $ f <$$> x

instance (Monoid e, Applicative u) => Applicative (UT ((,) e) u) where
	pure = UT . pure . pure
	UT f <*> UT x = UT $ f <**> x

instance (Monoid e, Applicative u, Monad u) => Monad (UT ((,) e) u) where
	UT x >>= f = UT $ x >>= \(acc, v) -> (\(acc', y) -> (acc <> acc', y)) <$> run (f v)

type Accumulated e t = Adaptable (Writer e) t

add :: Accumulated e t => e -> t ()
add s = adapt $ Writer (s, ())
