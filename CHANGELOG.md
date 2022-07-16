**Effect.Dist**
- Added pattern synonyms `SampPrj`, `SampDis`, `ObsPrj`, `ObsDis` for discharging/projecting `Sample` and `Observe` effects.
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
- `DiscreteUniform` -> `UniformD`

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
- `traceLPs` and `traceSamples`