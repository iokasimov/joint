module Control.Joint.Effects.State where

import "base" Control.Applicative (Alternative (empty, (<|>)))

import Control.Joint.Core (type (:.), type (:=))
import Control.Joint.Operators ((<$$>))
import Control.Joint.Abilities.Completable (Completable (complete))
import Control.Joint.Abilities.Interpreted (Interpreted (Primary, run))
import Control.Joint.Abilities.Transformer (Transformer (build, unite), Schema, (:>) (T))
import Control.Joint.Abilities.Adaptable (Adaptable (adapt))
import Control.Joint.Schemes (TUT (TUT), type (<:<.>:>))
import Control.Joint.Effects.Reader (Reader (Reader))
import Control.Joint.Effects.Writer (Writer (Writer))

newtype State s a = State ((->) s :. (,) s := a)

statefully :: s -> State s a -> (s, a)
statefully initial (State x) = x initial

instance Functor (State s) where
	fmap f (State x) = State $ \old -> f <$> x old

instance Applicative (State s) where
	pure x = State $ \s -> (s, x)
	State f <*> State x = State $ \old ->
		let (new, g) = f old in g <$> x new

instance Monad (State s) where
	State x >>= f = State $ \old ->
		uncurry statefully $ f <$> x old

instance Interpreted (State s) where
	type Primary (State s) a = (->) s :. (,) s := a
	run (State x) = x

type instance Schema (State s) = (->) s <:<.>:> (,) s

instance Transformer (State s) where
	build x = T . TUT $ pure <$> run x
	unite = T . TUT

instance Completable (Reader e) (State e) where
	complete (Reader f) = State (\e -> (e, f e))

instance Completable (Writer e) (State e) where
	complete (Writer (e, x)) = State $ \e -> (e, x)

type Stateful e = Adaptable (State e)

modify :: Stateful s t => (s -> s) -> t ()
modify f = adapt $ State $ \s -> (f s, ())

current :: Stateful s t => t s
current = adapt $ State $ \s -> (s, s)

replace :: Stateful s t => s -> t ()
replace new = adapt $ State $ \_ -> (new, ())
