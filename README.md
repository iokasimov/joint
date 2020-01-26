# Extremely simple effect system for Haskell

[Blogpost about](https://iokasimov.github.io/posts/2019/10/joint) | [Hackage documentation](http://hackage.haskell.org/package/joint) | [Example with brackets](https://gist.github.com/iokasimov/e149804f8bf4cb807a1ff6c2ae6a383a)

## Overview

`joint` let you type expressions by effects that they produce. No free/freer monad, no GADTs and other fancy stuff - all you need is a functor composition. If you want to be able to use your on effect in this library you need to pick a `joint schema` and write several instances (`Functor`/`Applicative`/`Monad`).

## Composing lifted effects

If you have some effectful expression, you can easy compose them:

```haskell
let f = lift get :: Configured _ t => t _
let g = lift Nothing :: Optional t => t _
let h = lift (failure _) :: Failable _ t => t _

let x = f *> g *> h :: (Applicative t, Configured _ t, Optional t, Failable _ t) => t _
```

## Fit effects in transformers

If you have concrete transformers, the order of effects doesn't matter, you can fit lifted effects to them:

```haskell
let y = pure _ :: Reader _ :> State _ :> Either _ :> Maybe := Int
let z = pure _ :: State _ :> Either _ :> Maybe _ :> Reader := _

let x = f *> g *> h :: (Applicative t, Configured _ t, Optional t, Failable _ t) => t _

let xy = x *> y :: Reader _ :> State _ :> Either _ :> Maybe := _
let xz = x *> z :: State _ :> Either _ :> Maybe _ :> Reader := _
```

## Running effects in transfomers

To interpret some effect you can use `run` method:

```haskell

let xy = x *> y :: Reader _ :> State _ :> Either _ :> Maybe := _

let xy' = run xy _ :: State _ :> Either _ :> Maybe := _
let xy'' = run xy' _ :: Either _ :> Maybe := _
let xy''' = run xy'' :: Maybe (Either _) _
```

## Effects adaptation

Adaptation means that some effects can be replaced by more powerful ones. For example, `Reader` and `Writer` effects can bu used in `State` because `State` can read and write, so it can modify stored value.

```haskell
lift put :: Accumulated _ t => t _
lift get :: Configured _ t => t _
(lift . adapt $ put) :: Stateful _ t => t _
(lift . adapt $ get) :: Stateful _ t => t _
```

So you can adapt `Failable` to `Optional` but we lost error information:

```haskell
(lift $ Just _) :: Optional t => t _
(lift $ failure _) :: Failable _ t => t _
(lift . adapt $ failure _) :: Optional t => t _
```
