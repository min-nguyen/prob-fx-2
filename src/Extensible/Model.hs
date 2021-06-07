{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Extensible.Model where

import Util
import Extensible.Dist
import Extensible.Freer
import Extensible.Reader
import Extensible.Sampler
import Extensible.IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head, Member)
import Control.Lens ( Identity, (^.), review, Getting )
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
-- import Control.Monad.Reader
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.Uniform
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC
import qualified Data.Vector as V
import Unsafe.Coerce

mkField "mu sigma y ys label"

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type MRec s = Record (Maybes s)

{- Probably too strict
-- type Model s rs a = Freer (Dist ': Reader (MRec s) ':  rs) a
-}
{- Only works with type applications
-- type Model s rs a = 
--   Member Dist rs => Member (Reader (MRec s)) rs => Freer rs a
-}

type Model env es a = 
  Member Dist es => (Freer (Reader (MRec env) ': es) a) 

access :: forall s rs a.
     Getting a (Maybes s :& Field Identity) a
  -> Model s rs a
access f =  do
    env :: MRec s <- ask
    return $ env ^. f
    
normal :: Member Dist rs => Double -> Double -> Model s rs Double
normal mu sigma = do
  send (NormalDist mu sigma Nothing) 

normal' :: forall s a rs. (a ~ Maybe Double) =>
   Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s rs Double
normal' mu sigma field = do
  maybe_y  <- access field
  send (NormalDist mu sigma maybe_y)

bernoulli :: (a ~ Maybe Bool)
  => Double -> Model s rs Bool
bernoulli p  = do
  send (BernoulliDist p Nothing) 

bernoulli' :: (a ~ Maybe Bool)
  => Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s rs Bool
bernoulli' p field = do
  y <- access field
  send (BernoulliDist p y)

gamma :: (a ~ Maybe Double)
  => Double -> Double -> Model s rs Double
gamma k θ  = do
  send (GammaDist k θ Nothing) 

gamma' :: (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s rs Double
gamma' k θ field = do
  y <- access field
  send (GammaDist k θ y)

-- data IfModel s rs a = IfModel 

-- if' :: forall s rs a. Bool -> Model s rs a -> Model s rs a -> Model s rs a
-- if' b (Free u@(Union n tx) k) m2 = 
--   case prj u :: forall x. Maybe (Dist x) of
--     _ -> undefined
  
  -- send $ IfDist b d1 d2


{- Newtype version of model (doesn't work with examples yet) -}
-- newtype Model env es a = Model { 
--     runModel ::  Member Dist es => Member (Reader (MRec env)) es => (Freer es a) 
--   } deriving (Functor)

-- instance Applicative (Model env es) where
--   pure x = Model (pure x)
--   (<*>) = ap

-- instance Monad (Model env es) where
--   return = pure 
--   Model fs >>= f = Model (fs >>= (runModel . f))

-- access :: forall s rs a.
--      Getting a (Maybes s :& Field Identity) a
--   -> Model s rs a
-- access f = Model $ do
--     env :: MRec s <- get
--     return $ env ^. f
    
-- normal :: Double -> Double -> Maybe Double -> Model s rs Double
-- normal mu sigma maybe_y = Model $ do
--   send (NormalDist mu sigma maybe_y)

-- normal' :: forall s a rs. (a ~ Maybe Double) =>
--    Double -> Double -> Getting a (Maybes s :& Field Identity) a
--   -> Model s rs Double
-- normal' mu sigma field = Model $ do
--   -- env :: MRec s <- get
--   maybe_y  <- runModel $ access field
--   send (NormalDist mu sigma maybe_y)
