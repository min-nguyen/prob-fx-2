## [1.0.0.0]

**examples**
Added:
- `GMM` (gaussian mixture model)
- `LinRegr`, `HMM`, `LDA`: examples of translating models to programs in `Monad Bayes`

**Effect.Dist**
Added:
- pattern synonyms `SampPrj`, `SampDis`, `ObsPrj`, `ObsDis` for discharging/projecting `Sample` and `Observe` effects.
Changed:
- `handleDist` now extends the effect signature with `Sample` and `Observe`, rather than assuming they are already part of the signature

**Effect.Lift**
Changed:
- `lift` now requires `m` to be the last member of the effect signature
Added:
- `liftM` for lifting `m` into `Model`

**Effect.ObsReader**
Changed:
- Renamed `handleRead` to `handleObsRead`

**Effect.State**
Added:
- `putM`, `getM`, and `handleStateM` for calling and handling `State` in the `Model` type

**Inference.LW**
Changed:
- Type signature for `lw`
Added:
- `lwInternal`

**Inference.MH**
Changed:
- Type signature for `mh`
- Type signature for `accept`
Added:
- `mhInternal`
- abstractions for accepting new proposals: `MHCtx`, `Accept`

**Env**
Changed:
- `ObsVar` -> `Var`, `UniqueKey` -> `UniqueVar`, `nil` -> `enil`
Added:
- `vnil`, `(<#>)`, `Vars`, `ContainsVars`, `GetVars`

**Model**
- Swapped names for `discrete` and `categorical`

**PrimDist**
Changed:
- Swapped names for `Discrete` and `Categorical`
- Removed the `Dist` suffix on the constructor names of `PrimDist`
- `DiscrUniform` -> `UniformD`

**PrimDist**
Changed:
- Swapped names for `Discrete` and `Categorical`
- Removed the `Dist` suffix on the constructor names of `PrimDist`
- `DiscrUniform` -> `UniformD`

**Prog**
Added:
- `weaken`, `install`, `UniqueMember`, `LastMember`

**Trace**
Changed:
- `updateLPTrace` and `updateSTrace` now invoke a `State` effect
Added:
- `traceLPs` and `traceSamples` handlers for recording log-probabilities and samples

