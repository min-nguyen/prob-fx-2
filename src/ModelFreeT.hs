
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE TypeApplications #-}
module ModelFreeT where

import Dist
import FreeT
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Extensible hiding (wrap, Head)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State

mkField "mu sigma y"

type Model s a = FreeT Dist (State (Record s)) a

data HList (l::[*]) :: * where
  HNil  :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup xs k (Maybe v) => HasVar xs k v  where

instance Lookup xs k (Maybe v) => HasVar xs k v where

type Params s = Record (Maybes s)

type Θ =
    '[  "mu"    ':>  Double
      , "sigma" ':>  Double
      , "y"     ':>  Double
     ]

exampleParams :: Params Θ
exampleParams = mu @= Just 5 <: sigma @= Just 2 <: y @= Just 0 <: emptyRecord

access :: Getting a (s :& Field Identity) a
       -> FreeT Dist (State (Record s)) a
access f = do
    state <- lift get
    return (state ^. f)

normal :: (a ~ Maybe Double)
       => Double -> Double
       -> Getting a (s :& Field Identity) a
       -> FreeT Dist (State (Record s)) Double
normal mu sigma field = do
    y <- access field
    suspend (NormalDist mu sigma y return)

runModelFree :: Model s a -> State (Record s) a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model

runModelState :: Model s a -> Record s -> (a, Record s)
runModelState model = runState (runModelFree model)

exampleModel :: (HasVar s "mu" Double) => Model s Double
exampleModel = do
  let r1 = 5
  x  <- normal 5 0 mu
  let r2 = 4
  return (r1 + r2)

linearRegression :: (HasVar s "y" Double) =>
                    Double -> Double -> Double -> Model s Double
linearRegression muu sigma x = do
  y' <- normal (muu + x) sigma y
  return y'

runExample :: (Double, Params Θ)
runExample = runModelState exampleModel exampleParams
