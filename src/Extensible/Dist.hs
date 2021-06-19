{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Extensible.Dist where

-- import Extensible.IO
import Extensible.Freer
import Extensible.Sampler
import Util
import Control.Lens hiding ((:>))
import Control.Monad.State
import Data.Kind
import Data.Maybe
import Data.Extensible hiding (wrap, Head, Member)
import qualified Data.Vector as V
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.Uniform
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC


data DistInfo where
  NormalDistI        :: Double -> Double -> DistInfo
  -- uniform        :: Dist Double
  UniformDistI       :: Double -> Double -> DistInfo
  -- discr uniform  :: Dist Int
  DiscrUniformDistI  :: Int    -> Int    -> DistInfo
  -- gamma          :: Dist Double
  GammaDistI         :: Double -> Double -> DistInfo
  -- beta           :: Dist Double
  BetaDistI          :: Double -> Double -> DistInfo
  -- binomial       :: Dist [Bool]
  BinomialDistI      :: Int    -> Double -> DistInfo
  -- bernoulli      :: Dist Bool
  BernoulliDistI     :: Double -> DistInfo
  -- categorical    :: Dist Int
  CategoricalDistI   :: V.Vector (Int, Double) -> DistInfo
  -- discrete       :: Dist Int
  DiscreteDistI      :: [(Int, Double)] -> DistInfo
  deriving Eq

instance Show DistInfo where
  show (NormalDistI mu sigma ) =
    "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ")"
  show (BernoulliDistI p ) =
    "BernoulliDist(" ++ show p  ++ ")"
  show (DiscreteDistI ps ) =
    "DiscreteDist(" ++ show ps ++ ")"
  show (BetaDistI a b ) =
    "DiscreteDist(" ++ show a ++ ", " ++ show b ++ ")"
  show (GammaDistI a b ) =
    "GammaDist(" ++ show a ++ ", " ++ show b ++ ")"

toDistInfo :: Dist a -> DistInfo
toDistInfo (NormalDist mu sigma y) = NormalDistI mu sigma
toDistInfo (UniformDist min max y) = UniformDistI min max
toDistInfo (DiscrUniformDist min max y) = DiscrUniformDistI min max
toDistInfo (GammaDist a b y) = GammaDistI a b
toDistInfo (BetaDist a b y) = BetaDistI a b
toDistInfo (BinomialDist n p y) = BinomialDistI n p
toDistInfo (BernoulliDist p y) = BernoulliDistI p

data Dist a where
  -- IfDist            :: Bool -> Dist a -> Dist a -> Dist a
  -- normal         :: Dist Double
  NormalDist        :: Double -> Double -> Maybe Double -> Dist Double
  -- uniform        :: Dist Double
  UniformDist       :: Double -> Double -> Maybe Double -> Dist Double
  -- discr uniform  :: Dist Int
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> Dist Int
  -- gamma          :: Dist Double
  GammaDist         :: Double -> Double -> Maybe Double -> Dist Double
  -- beta           :: Dist Double
  BetaDist          :: Double -> Double -> Maybe Double -> Dist Double
  -- binomial       :: Dist [Bool]
  BinomialDist      :: Int    -> Double -> Maybe Int -> Dist Int
  -- bernoulli      :: Dist Bool
  BernoulliDist     :: Double -> Maybe Bool -> Dist Bool
  -- categorical    :: Dist Int
  CategoricalDist   :: V.Vector (Int, Double) -> Maybe Int -> Dist Int
  -- discrete       :: Dist Int
  DiscreteDist      :: [(Int, Double)] -> Maybe Int -> Dist Int

instance Show a => Show (Dist a) where
  show (NormalDist mu sigma y) =
    "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ show y ++ ")"
  show (BernoulliDist p y) =
    "BernoulliDist(" ++ show p ++ ", " ++ show y ++ ")"
  show (DiscreteDist ps y) =
    "DiscreteDist(" ++ show ps ++ ", " ++ show y ++ ")"
  show (BetaDist a b y) =
    "DiscreteDist(" ++ show a ++ ", " ++ show b ++ "," ++ show y ++ ")"
  show (GammaDist a b y) =
    "GammaDist(" ++ show a ++ ", " ++ show b ++ "," ++ show y ++ ")"

type Addr = Int

data Sample a where
  Sample :: Dist a -> Addr -> Sample a

data Observe a where
  Observe :: Dist a -> a -> Addr -> Observe a

pattern Samp :: Member Sample rs => Dist x -> Addr -> Union rs x
pattern Samp d α <- (prj -> Just (Sample d α))

pattern Obs :: Member Observe rs => Dist x -> x -> Addr -> Union rs x
pattern Obs d y α <- (prj -> Just (Observe d y α))

pattern DistDouble :: Dist Double -> Dist x
pattern DistDouble d <- (isDistDouble -> d)
pattern DistBool :: Dist Bool -> Dist x
pattern DistBool d <- (isDistBool -> d)
pattern DistInt :: Dist Int -> Dist x
pattern DistInt d <- (isDistInt -> d)

isDistDouble :: Dist x -> Dist Double
isDistDouble d@NormalDist {} = d
isDistDouble d@BetaDist {} = d
isDistDouble d@GammaDist {} = d
isDistDouble d@UniformDist {} = d

isDistBool :: Dist x -> Dist Bool
isDistBool d@BernoulliDist {} = d

isDistInt :: Dist x -> Dist Int
isDistInt d@DiscrUniformDist {} = d
isDistInt d@BinomialDist {} = d
isDistInt d@CategoricalDist {} = d
isDistInt d@DiscreteDist {} = d

{- Replaces Dists with Sample or Observe and adds address -}
runDist :: forall rs a. (Member Sample rs, Member Observe rs) => Freer (Dist : rs) a
        -> Freer rs a
runDist = loop 0
  where
  loop :: (Member Sample rs, Member Observe rs) => Addr -> Freer (Dist : rs) a -> Freer rs a
  loop α (Pure x) = return x
  loop α (Free u k) = case decomp u of
    Right d
      -> if hasObs d
          then send (Observe d (getObs d) α) >>= loop (α + 1) . k
          else send (Sample d α) >>= loop (α + 1). k
    Left  u'  -> Free u' (loop α . k)

instance Distribution (Dist a) where
  cumulative (NormalDist μ σ _ ) x
    = cumulative (normalDistr μ σ) x
  cumulative (UniformDist min max obs ) x
    = cumulative (uniformDistr min max) x
  cumulative (GammaDist k θ _) x
    = cumulative (gammaDistr k θ) x
  cumulative (BetaDist α β _) x
    = cumulative (betaDistr α β) x
  cumulative (BinomialDist n p _ ) x
    = cumulative (binomial n p) x
  cumulative (BernoulliDist p _ ) x
    = cumulative (binomial 1 p) x
  cumulative (CategoricalDist ps _ ) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps
  cumulative (DiscreteDist ps _ ) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps

instance ContDistr (Dist a) where
  density (NormalDist μ σ _ )          = density (normalDistr μ σ)
  density (UniformDist min max _ )     = density (uniformDistr min max)
  density (GammaDist k θ _ )           = density (gammaDistr k θ)
  density (BetaDist α β _ )            = density (betaDistr α β)
  logDensity (NormalDist μ σ _ )       = logDensity (normalDistr μ σ)
  logDensity (UniformDist min max _ )  = logDensity (uniformDistr min max)
  logDensity (GammaDist k θ _ )        = logDensity (gammaDistr k θ)
  logDensity (BetaDist α β _ )         = logDensity (betaDistr α β)
  quantile (NormalDist μ σ _ )         = quantile (normalDistr μ σ)
  quantile (UniformDist min max _ )    = quantile (uniformDistr min max)
  quantile (GammaDist k θ _ )          = quantile (gammaDistr k θ)
  quantile (BetaDist α β _ )           = quantile (betaDistr α β)

instance DiscreteDistr (Dist a) where
  -- binomial: probability of `i` successful outcomes
  probability (BinomialDist n p _ ) i            = probability (binomial n p) i
  probability (BernoulliDist p _ ) i             = probability (binomial 1 p) i
  probability (CategoricalDist ps _ ) i          = snd (ps V.! i)
  probability (DiscreteDist ps _ ) i             = snd (ps !! i)
  probability (DiscrUniformDist min max _ ) i    = probability (discreteUniformAB min max) i
  logProbability (BinomialDist n p _ ) i         = logProbability (binomial n p) i
  logProbability (BernoulliDist p _ ) i          = logProbability (binomial 1 p) i
  logProbability (CategoricalDist ps _ ) i       = (log . snd) (ps V.! i)
  logProbability (DiscreteDist ps _ ) i          = (log . snd) (ps !! i)
  logProbability (DiscrUniformDist min max _ ) i = logProbability (discreteUniformAB min max) i

hasObs :: Dist a -> Bool
hasObs d@(NormalDist _ _ obs )       = isJust obs
hasObs d@(DiscrUniformDist _ _ obs ) = isJust obs
hasObs d@(UniformDist _ _ obs )      = isJust obs
hasObs d@(GammaDist _ _ obs )        = isJust obs
hasObs d@(BetaDist _ _ obs )         = isJust obs
hasObs d@(BinomialDist _ _ obs )     = isJust obs
hasObs d@(BernoulliDist _ obs )      = isJust obs
hasObs d@(CategoricalDist _ obs )    = isJust obs
hasObs d@(DiscreteDist _ obs )       = isJust obs

getObs :: Dist a -> a
getObs d@(NormalDist _ _ obs )       = fromJust obs
getObs d@(DiscrUniformDist _ _ obs ) = fromJust obs
getObs d@(UniformDist _ _ obs )      = fromJust obs
getObs d@(GammaDist _ _ obs )        = fromJust obs
getObs d@(BetaDist _ _ obs )         = fromJust obs
getObs d@(BinomialDist _ _ obs )     = fromJust obs
getObs d@(BernoulliDist _ obs )      = fromJust obs
getObs d@(CategoricalDist _ obs )    = fromJust obs
getObs d@(DiscreteDist _ obs )       = fromJust obs

prob :: Dist a -> a -> Double
prob d@NormalDist {} y
  = density d y
prob d@DiscrUniformDist {} y
  = probability d y
prob d@UniformDist {} y
  = density d y
prob d@GammaDist {} y
  = density d y
prob d@BetaDist {}  y
  = density d y
prob d@BinomialDist {} y
  = probability d y
prob d@BernoulliDist {} y
  = probability d (boolToInt y)
prob d@CategoricalDist {} y
  = probability d y
prob d@DiscreteDist {} y
  = probability d y

{- Combines logDensity and logProbability.
   Log probability of double `x` from a distribution -}
logProb :: Dist a -> a -> Double
logProb d = log . prob d

sample :: Dist a -> Sampler a
sample (NormalDist μ σ obs)  =
  createSampler (sampleNormal μ σ) >>= return
sample (UniformDist min max obs  )  =
  createSampler (sampleUniform min max) >>= return
sample (DiscrUniformDist min max obs )  =
  createSampler (sampleDiscreteUniform min max) >>= return
sample (GammaDist k θ obs )        =
  createSampler (sampleGamma k θ) >>= return
sample (BetaDist α β  obs )         =
  createSampler (sampleBeta α β) >>= return
sample (BinomialDist n p  obs )     =
  createSampler (sampleBinomial n p) >>=  return .  length
sample (BernoulliDist p obs )      =
  createSampler (sampleBernoulli p) >>= return
sample (CategoricalDist ps obs )   =
  createSampler (sampleCategorical (fmap snd ps)) >>= return
sample (DiscreteDist ps obs )      =
  createSampler (sampleDiscrete (map snd ps)) >>= return


{- Old version of runDist which always samples -}
-- runDist :: Member (Lift Sampler) rs => Freer (Dist : rs) a
--         -> Freer rs  a
-- runDist m = loop m where
--   loop :: Member (Lift Sampler) rs => Freer (Dist : rs) a -> Freer rs a
--   loop (Pure x) = return x
--   loop (Free u k) = case decomp u of
--     Right d@(NormalDist m sigma y)
--       -> send (Lift (sample d)) >>= loop . k
--     Left  u'  -> Free u' (loop . k)
