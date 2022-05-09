
# models

- **constr-kinds**: Replaced use of some pattern synonyms (previously used to match against existentially quantified 'x' in `Dist x`) with constraint kinds instead as a proof that the parameter of `Dist x` must be a member of `PrimVals`.

Old:
```
isDistBool :: Dist x -> Maybe (Dist Bool)
isDistBool d@BernoulliDist {} = Just d
isDistBool _ = Nothing

pattern DistBool :: Dist Bool -> Dist x
pattern DistBool d <- (isDistBool -> Just d)

pattern SampBoolPrj :: Member Sample ts => Dist Bool -> Addr -> Union ts x
pattern SampBoolPrj d α <- (prj -> Just (Sample (DistBool  d) α))

transformMH (Free u k) = do
  case u of
    SampBoolPrj d α -> Free u (\x -> modify (updateSMap α d (unsafeCoerce x :: Bool)) >>
                                     modify (updateLPMap α d (unsafeCoerce x :: Bool)) >>
                                     transformMH (k x))
```

New:
```

type PrimVal = '[Int, Double, [Double], Bool, String]

data PrimDist where
  PrimDist :: forall a. Show a => Dist a -> PrimDist

data Dict (a :: Constraint) where
  Dict :: a => Dict a

distDict :: Dist x -> Dict (Show x, OpenSum.Member x PrimVal)
distDict = \case
  HalfCauchyDist {} -> Dict
  CauchyDist {} -> Dict
  NormalDist {} -> Dict
  ...
 
pattern DistDict :: () => (Show x, OpenSum.Member x PrimVal) => Dist x -> Dist x
pattern DistDict d <- d@(distDict -> Dict)

pattern Samp :: Member Sample rs => Dist x -> Addr -> Union rs x
pattern Samp d α <- (prj -> Just (Sample d α))

pattern SampPatt :: (Member Sample rs) => (Show x, OpenSum.Member x PrimVal) => Dist x -> Addr -> Union rs x
pattern SampPatt d α <- (Samp (DistDict d) α)

transformMH (Free u k) = do
  case u of
    Samp d α -> case distDict d of
                  Dict -> Free u (\x -> modify (updateSMap α d x) >>
                                        modify (updateLPMap α d (unsafeCoerce x)) >>
                                        transformMH (k x))

```

- **dep-map**: Extended version of `effect-abstractions-constr-kinds` which uses dependent maps `DMap (Key Addr)` as sample maps, as opposed to `Map Addr (OpenSum' PrimVal)`.

- **icfp22-version**: Version for ICFP22 submission

- **inference-extension** for extending inference
