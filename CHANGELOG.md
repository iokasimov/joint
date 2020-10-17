# 0.1.1
* Define `UT` joint scheme
* Define joint scheme for `Maybe` datatype
* Define joint scheme for `Either` datatype
* Define joint scheme for `Reader` datatype

# 0.1.2
* Define `Functor`/`Applicative`/`Monad` instances for transformers
* Define `State` datatype with `get`, `modify` and `put` operations
* Rename and fix `UTU` joint scheme to `TUT`
* Renaming: `unwrap` -> `run`, `wrap` -> `build`, `lay` -> `embed`

# 0.1.3
* Define `unite` method to convert composition to transformer
* Define `Modulator` class to map function over inner effects
* Rename `Reader` datatype to `Configured`
* Add `ask` operation for `Configured` effect

# 0.1.4
* Define `Liftable` class to lift many effects in schemes and other effects
* Create `Abilities` and `Effects` umbrella modules
* Rename `Configured` datatype back to `Reader`
* Define `Failable`, `Optional`, `Configured` and `Stateful` constraints
* Convert `:>` to a newtype

# 0.1.5
* Define `Adaptable` class to transform some effect to others
* Define `Writer` datatype and move `put` from `State` to `Writer`
* Rename `ask` to `get` and move it to `Reader` effect
* Add `failure` operation for `Either` effect
* Methods of `Transformer` class works on `:>` datatype
* Remove `Modulator` module and class

# 0.1.6
* Lower precedence of `:>` operator to allow write like: `t :> u := a`
* Create supermodule to have only one line of import in projects
* Rename `Composition` class to `Interpreted`
* Rename `put` operation for `Writer` to `add`
* Add `nothing` operation for `Maybe` effect
* Add `current` operation for `State` effect
* Make `get`, `add`, `current`, `modify`, `nothing`, `failure` liftable

# 0.1.7
* Create `Operators` module to work with nested functor compositions
* Add experimental `$>>=` and `>>=$` monadic operators
* Define `replace` method in `Effects.State` module
* Change order of parameters in `TUT` newtype schema
* Change arity of `Schema` associated type family
* Add `transformers` dependency

# 0.1.8
* Rename `Adaptable` class to `Completable` and `adapt` to `complete`
* Rename `Liftable` class to `Adaptable` and `lift` to `adapt`
* Define `MonadTrans` instances for `TU` and `UT` schemes
* Define `Store` datatype and define `Functor` and `Comonad` instances for it
* Extract `Schema` associated type family from `Transformer` typeclass
* Replace `embed` method with `lift` from `MonadTrans` typeclass
* Define `ComonadTrans` instances for `TU` and `UT` schemes

# 0.1.9
* Define `pos`, `seek`, `peek` and `retrofit` methods for `Store`
