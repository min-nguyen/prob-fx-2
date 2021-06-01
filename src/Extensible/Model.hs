{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module Extensible.Model where

import Sample
import Util
import Extensible.Dist
import Extensible.Freer
import Extensible.Reader
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head, Member)
import Control.Lens ( Identity, (^.), review, Getting )
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
-- import Control.Monad.Reader

mkField "mu sigma y ys"

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type MRec s = Record (Maybes s)

type Model s rs a =
  Freer (Dist ': Reader (MRec s) ': Sampler ': rs) a

access :: forall s rs a.  
     Getting a (Maybes s :& Field Identity) a
  -> Model s rs a
access f = do
    env :: MRec s <- get
    return $ env ^. f

normal :: Double -> Double -> Maybe Double 
       -> Model s rs Double 
normal mu sigma maybe_y = do
  send (NormalDist mu sigma maybe_y)

normal' :: (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s rs Double
normal' mu sigma field = do
  maybe_y <- access field
  send (NormalDist mu sigma maybe_y )
