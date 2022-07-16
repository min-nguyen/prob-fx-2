**Effect.Dist**
- Added pattern synonyms `SampPrj`, `SampDis`, `ObsPrj`, `ObsDis` for discharging/projecting `Sample` and `Observe` effects.

**Effect.Lift**
Changed:
- `lift` now requires `m` to be the last member of the effect signature
Added:
- `liftM` for lifting `m` into `Model`

**Env**
Changed:
- `ObsVar` -> `Var`
- `UniqueKey` -> `UniqueVar`
- `nil` -> `enil`
Added: 
- `vnil`
- `(<#>)`
- `Vars`
- `ContainsVars`

**Model**
- Swapped names for `discrete` and `categorical` 

**PrimDist**
Changed:
- Removed the `Dist` suffix on the constructor names of `PrimDist`
- Swapped names for `Discrete` and `Categorical` 
- `DiscreteUniform` -> `UniformD`