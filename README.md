# models

Branches:

- **reader-env**: Models with a `Reader env` effect, where observable variables in the environment correspond to a value of type `Maybe a`.
```
newtype Model env es a = Model { runModel :: (Member Dist es, Member (Reader (MRec env)) es) => Freer es a }

normal' :: forall s es a . (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s es Double
normal' mu sigma field = Model $ do
  env <- accessEnv
  let maybe_y = env ^. field
  send (NormalDist mu sigma maybe_y)
```
 
- **affreader-env**: An improved version of reader-env where models instead have an `AffReader env` effect, where observable variables are allowed to point to multiple values in a list, and observing a variable permanently consumes a value from the head of the list. The definition of models also includes a `Sample` effect which is purely to allow debugging via print statements (where printing is a Sample operation). Additionally, more sophisticated addresses are introduced: observable variable names are converted to tags to use as addresses to distribution calls, which is incorporated in the definition of the distribution handler `runDist`. Observable variable names are also converted to lenses to allow the affine reader effect to apply `get` and `set` operations on the environment. We also introduce categorical and deterministic distributions whose types range over any primitive value type.
```
newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (AffReader (OP.AsList env)) es, Member Sample es) => Freer es a }

normal' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> Double -> OP.Key k
  -> Model s es Double
normal' mu sigma field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (NormalDist mu sigma maybe_y tag)
  
data AffReader env a where
  Ask :: Getting [a] (OP.OpenProduct env) [a]
      -> ASetter (OP.OpenProduct env) (OP.OpenProduct env) [a] [a]
      -> AffReader env (Maybe a)

ask :: Member (AffReader env) rs => Getting [a] (OP.OpenProduct env) [a]
  -> ASetter (OP.OpenProduct env) (OP.OpenProduct env) [a] [a]
  -> Freer rs (Maybe a)
ask g s = Free (inj $ Ask g s) Pure

-- | The only purpose of the State (LRec env) effect is to check if all observed values in the environment have been consumed.
runAffReader :: forall env rs a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader (OP.AsList env) ': rs) a -> Freer rs (a, OP.OpenProduct (OP.AsList env) )
runAffReader env = loop env where
  loop :: OP.OpenProduct (OP.AsList env) -> Freer (AffReader (OP.AsList env)  ': rs) a -> Freer rs (a, OP.OpenProduct (OP.AsList env) )
  loop env (Pure x) = return (x, env)
  loop env (Free u k) = case decomp u of
    Right (Ask g s) -> do
      let ys = env ^. g
          y  = maybeHead ys
          env' = env & s .~ safeTail ys
      loop env' (k y)
    Left  u'  -> Free u' (loop env . k)
```

- **paper-version**: A cleaned up version of affreader-env for IFL 21. A main implementation difference is that the `HasVar` constraint is renamed to `Observable`, and observable variable names are also no longer converted to lenses; this requires a new definition for `AffReader` where we instead we directly use the variable names with the functions `getOP` and `setOP` on the environment. Additionally, we no longer use the boilerplatey data type `DistInfo` to store distribution information from the gadt `Dist a` in a map during Metropolis-Hastings; instead, we use an existential data type `PrimDist` to wrap the distributions.
```
newtype Model env ts v =
  Model { runModel :: (Member Dist ts, Member (AffReader env) ts, Member Sample ts) => Freer ts v }

normal' :: forall env ts x. OP.Lookup (OP.AsList env) x [Double]
  => Double -> Double -> OP.Var x
  -> Model env ts Double
normal' mu sigma field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (NormalDist mu sigma maybe_y tag)

data AffReader env a where
  Ask :: OP.Observable env x a => OP.Var x -> AffReader env (Maybe a)

ask :: forall env ts x a. Member (AffReader env) ts => OP.Observable env x a => OP.Var x -> Freer ts (Maybe a)
ask k = Free (inj (Ask k :: AffReader env (Maybe a))) Pure

runAffReader :: forall env ts a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader env ': ts) a -> Freer ts a
runAffReader env (Pure x) = return x
runAffReader env (Free u k) = case decomp u of
  Right (Ask key) -> do
    let ys = OP.getOP key env
        y  = maybeHead ys
        env' = OP.setOP key (safeTail ys) env
    runAffReader env' (k y)
  Left  u'  -> Free u' (runAffReader env . k)
  
data PrimDist where
  PrimDist :: (forall a. Show a => Dist a -> PrimDist)
```


- **effect-abstractions**: An extended version of paper-version where effect handler abstractions are introduced, such as handleRelay, interpose, etc. Some existing effect handlers and inference handlers are also given alternative definitions where these handler abstractions are used.
```
replaceRelaySt ::
      s
  ->  (s -> a -> Freer (v ': ts) b)
  ->  (forall x. s -> t x -> (s -> x -> Freer (v ': ts) b) -> Freer (v ': ts) b)
  ->  Freer (t ': ts) a
  ->  Freer (v ': ts) b
replaceRelaySt s ret h (Pure x) = ret s x
replaceRelaySt s ret h (Free u k) = case decomp u of
  Right tx -> h s tx (\s' x -> replaceRelaySt s' ret h $ k x)
  Left  u' -> Free (weaken u') (replaceRelaySt s ret h . k)

runAffReader :: forall env ts a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader env ': ts) a -> Freer ts a
runAffReader env0 = handleRelaySt env0
  (\env x    -> return x)
  (\env tx k ->
    case tx of
      Ask key -> let ys   = OP.getOP key env
                     y    = maybeHead ys
                     env' = OP.setOP key (safeTail ys) env
                 in  k env' y)
```
