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
