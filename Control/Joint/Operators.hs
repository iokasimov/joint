module Control.Joint.Operators where

import Control.Joint.Core ((:.), (:=))

(<$$>) :: (Functor t, Functor u) => (a -> b) -> t :. u := a -> t :. u := b
(<$$>) = (<$>) . (<$>)

(<$$$>) :: (Functor t, Functor u, Functor v)
	=> (a -> b) -> t :. u :. v := a -> t :. u :. v := b
(<$$$>) = (<$>) . (<$>) . (<$>)

(<$$$$>) :: (Functor t, Functor u, Functor v, Functor w)
	=> (a -> b) -> t :. u :. v :. w := a -> t :. u :. v :. w := b
(<$$$$>) = (<$>) . (<$>) . (<$>) . (<$>)


(<**>) :: (Applicative t, Applicative u) => t :. u := (a -> b) -> t :. u := a -> t :. u := b
f <**> x = (<*>) <$> f <*> x

(<***>) :: (Applicative t, Applicative u, Applicative v) => t :. u :. v := (a -> b)
	-> t :. u :. v := a -> t :. u :. v := b
f <***> x = (<**>) <$> f <*> x

(<****>) :: (Applicative t, Applicative u, Applicative v, Applicative w)
	=> t :. u :. v :. w := (a -> b)
	-> t :. u :. v :. w := a
	-> t :. u :. v :. w := b
f <****> x = (<***>) <$> f <*> x
