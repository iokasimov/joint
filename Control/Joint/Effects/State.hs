module Control.Joint.Effects.State where

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Abilities.Composition (Composition (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (Schema, embed, build, unite))
import Control.Joint.Abilities.Liftable (Liftable (lift))
import Control.Joint.Schemes.TU (TU (TU))
import Control.Joint.Schemes.TUT (TUT (TUT))
import Control.Joint.Schemes.UT (UT (UT))
import Control.Joint.Effects.Reader (Reader (Reader))

newtype State s a = State ((->) s :. (,) s := a)

statefully :: s -> State s a -> (s, a)
statefully initial (State x) = x initial

instance Functor (State s) where
	fmap f (State x) = State $ \old -> f <$> x old

instance Applicative (State s) where
	pure x = State $ \s -> (s, x)
	State f <*> State x = State $ \old ->
		let latest = fst . x $ old in
			(latest, snd (f old) . snd . x $ old)

instance Monad (State s) where
	State x >>= f = State $ \old ->
		uncurry statefully $ f <$> x old

instance Composition (State s) where
	type Primary (State s) a = (->) s :. (,) s := a
	run (State x) = x

instance Transformer (State s) where
	type Schema (State s) u = TUT ((->) s) u ((,) s)
	embed x = TUT $ \s -> (s,) <$> x
	build x = TUT $ pure <$> run x
	unite = TUT

instance Functor u => Functor (TUT ((->) s) u ((,) s)) where
	fmap f (TUT x) = TUT $ \old -> (fmap . fmap) f $ x old

instance Monad u => Applicative (TUT ((->) s) u ((,) s)) where
	pure x = TUT $ \s -> pure (s, x)
	TUT f <*> TUT x = TUT $ \old -> f old >>= \(new, g) -> (fmap . fmap) g $ x new

instance Monad u => Monad (TUT ((->) s) u ((,) s)) where
	TUT x >>= f = TUT $ \old -> x old >>= \(new, y) -> ($ new) . run . f $ y

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

put :: s -> State s ()
put s = State $ \_ -> (s, ())

instance Applicative u => Liftable (State s) (TUT ((->) s) u ((,) s)) where
	lift (State x) = TUT $ pure <$> x

instance Liftable (State s) u => Liftable (State s) (TUT t u t') where
	lift = lift

instance Liftable (State s) u => Liftable (State s) (TU t u) where
	lift = lift

instance Liftable (State s) u => Liftable (State s) (UT t u) where
	lift = lift

instance Liftable (Reader e) (State e) where
	lift (Reader f) = State (\e -> (e, f e))

type Stateful e t = Liftable (State e)
