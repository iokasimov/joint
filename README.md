# Extremely simple effect system for Haskell

[Hackage documentation](http://hackage.haskell.org/package/joint) | [Examples](https://github.com/iokasimov/experiments)

## Overview

`joint` let you type expressions by effects that they produce. No free/freer monad, no GADTs and other fancy stuff - all you need is a functor composition. If you want to be able to use your on effect in this library you need to pick a `joint schema` and write several instances (`Functor`/`Applicative`/`Monad`). For better understanding it's recommend to read this explanation series: [part 1](https://iokasimov.github.io/posts/2019/11/joint), [part 2](https://iokasimov.github.io/posts/2020/02/joint), [part 3](https://iokasimov.github.io/posts/2021/01/composable-monad-transformers).


## Simple real world example

Let’s imagine that we need to make an HTTP request, it’s `IO`, that can throw `HttpException`:

```haskell
import qualified "wreq" Network.Wreq as HTTP

request :: (Monad t, Liftable IO t, Failable HttpException t) => t (Response ByteString)
request = lift (try @HttpException $ HTTP.get link) >>= lift
```

Wow, what is there? First, we `lift` some `IO`-action, then after `>>=` we `lift` `Either` and we get an expression, that can be used in many effectful expressions than contain such two effects! We can delay using concrete transformers until we really need to evaluate them.

## Composing lifted effects

If you have some effectful expression, you can easy compose them:

```haskell
let f = get :: Configured _ t => t _
let g = nothing :: Optional t => t _
let h = failure _ :: Failable _ t => t _

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
