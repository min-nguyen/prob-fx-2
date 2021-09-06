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
 
- **affreader-env**: An improved version of reader-env where models instead have an `AffReader env` effect, where observable variables are allowed to point to multiple values in a list, and observing a variable permanently consumes a value from the head of the list. The definition of models also includes a `Sample` effect which is purely to allow debugging via print statements (where printing is a Sample operation). Additionally, observable variable names are converted to tags to use as addresses to distribution calls, and they are also converted to lenses to allow the affine reader effect to apply `get` and `set` operations on the environment. We also introduce categorical and deterministic distributions whose types range over any primitive value type.
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
```

- **paper-version**: A cleaned up version of affreader-env, where observable variable names are also no longer converted to lenses.
```
newtype Model env ts v =
  Model { runModel :: (Member Dist ts, Member (AffReader env) ts, Member Sample ts) => Freer ts v }
```


- **effect-abstractions**: An extended version of paper-version where effect handler abstractions are introduced, such as handleRelay, interpose, etc.
