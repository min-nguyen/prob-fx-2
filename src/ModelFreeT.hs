
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module ModelFreeT where

import Dist
import FreeT
import Util
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Extensible hiding (wrap, Head)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.Reader

mkField "mu sigma y ys"

type Model s a = FreeT Dist (Reader (Record s)) a

{- Handling Accessible Model Variables -}
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

type Vars s = Record (Maybes s)

type X =
    '[  "mu"    ':>  Double
      , "sigma" ':>  Double
      , "y"     ':>  Double
     ]

exampleParams :: Vars X
exampleParams = mu @= Just 5 <: sigma @= Just 2 <: y @= Just 0 <: emptyRecord

access :: Getting a (s :& Field Identity) a
  -> FreeT Dist (Reader (Record s)) a
access f = do
    env <- lift ask
    return (env ^. f)

{- Distribution functions -}
normal :: (a ~ Maybe Double)
  => Double -> Double -> Maybe Double 
  -> FreeT Dist (Reader (Record s)) Double
normal mu sigma maybe_y = do
  suspend (NormalDist mu sigma maybe_y return)

normal' :: (a ~ Maybe Double)
  => Double -> Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (Reader (Record s)) Double
normal' mu sigma field = do
  y <- access field
  suspend (NormalDist mu sigma y return)

bernoulli :: (a ~ Maybe Bool)
  => Double -> Maybe Bool
  -> FreeT Dist (Reader (Record s)) Bool
bernoulli p maybe_y = do
  suspend (BernoulliDist p maybe_y return)

bernoulli' :: (a ~ Maybe Bool)
  => Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (Reader (Record s)) Bool
bernoulli' p field = do
  y <- access field
  suspend (BernoulliDist p y return)

binomial :: (a ~ Maybe Int)
  => Int -> Double -> Maybe Int
  -> FreeT Dist (Reader (Record s)) Int
binomial n p maybe_y = do
  suspend (BinomialDist n p maybe_y return)

binomial' :: (a ~ Maybe Int)
  => Int -> Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (Reader (Record s)) Int
binomial' n p field = do
  y <- access field
  suspend (BinomialDist n p y return)

{- Executing Models -}
runModelFree :: Model s a -> Reader (Record s) a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model

runModel :: Model s a -> Record s -> a
runModel model = runReader (runModelFree model)