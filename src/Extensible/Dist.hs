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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Extensible hiding (wrap, Head, Member)
import qualified Data.Vector as V
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Poisson
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
  -- poisson        :: Dist Int
  PoissonDistI       :: Double -> DistInfo
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
  show (UniformDistI a b ) =
    "UniformDist(" ++ show a ++ ", " ++ show b ++ ")"
  show (DiscrUniformDistI min max ) =
    "DiscrUniformDist(" ++ show min ++ ", " ++ show max ++ ")"
  show (PoissonDistI l) =
    "PoissonDist(" ++ show l ++ ")"
  show (CategoricalDistI xs ) =
    "CategoricalDist(" ++ show xs  ++ ")"
  show (BinomialDistI n p ) =
    "BinomialDist(" ++ show n ++ ", " ++ show p ++ ")"

toDistInfo :: Dist a -> DistInfo
toDistInfo (NormalDist mu sigma y tag) = NormalDistI mu sigma
toDistInfo (UniformDist min max y tag) = UniformDistI min max
toDistInfo (DiscrUniformDist min max y tag) = DiscrUniformDistI min max
toDistInfo (GammaDist a b y tag) = GammaDistI a b
toDistInfo (BetaDist a b y tag) = BetaDistI a b
toDistInfo (BinomialDist n p y tag) = BinomialDistI n p
toDistInfo (BernoulliDist p y tag) = BernoulliDistI p
toDistInfo (PoissonDist l y tag) = PoissonDistI l
toDistInfo (DiscreteDist p y tag) = DiscreteDistI p
toDistInfo (CategoricalDist p y tag) = CategoricalDistI p

data Dist a where
  -- IfDist            :: Bool -> Dist a -> Dist a -> Dist a
  -- normal         :: Dist Double
  NormalDist        :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  -- uniform        :: Dist Double
  UniformDist       :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  -- discr uniform  :: Dist Int
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> Maybe String -> Dist Int
  -- gamma          :: Dist Double
  GammaDist         :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  -- beta           :: Dist Double
  BetaDist          :: Double -> Double -> Maybe Double -> Maybe String ->  Dist Double
  -- binomial       :: Dist [Bool]
  BinomialDist      :: Int    -> Double -> Maybe Int -> Maybe String -> Dist Int
  -- bernoulli      :: Dist Bool
  BernoulliDist     :: Double -> Maybe Bool -> Maybe String -> Dist Bool
  -- categorical    :: Dist Int
  CategoricalDist   :: V.Vector (Int, Double) -> Maybe Int -> Maybe String -> Dist Int
  -- discrete       :: Dist Int
  DiscreteDist      :: [(Int, Double)] -> Maybe Int -> Maybe String -> Dist Int
  -- poisson        :: Dist Int
  PoissonDist       :: Double -> Maybe Int -> Maybe String -> Dist Int

instance Show a => Show (Dist a) where
  show (NormalDist mu sigma y tag) =
    "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (BernoulliDist p y tag) =
    "BernoulliDist(" ++ show p ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (BinomialDist n p y tag) =
    "BinomialDist(" ++ show n ++ ", " ++ show p ++ ", " ++ show y ++ ", " ++ show tag ++  ")"
  show (DiscreteDist ps y tag) =
    "DiscreteDist(" ++ show ps ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (BetaDist a b y tag) =
    "BetaDist(" ++ show a ++ ", " ++ show b ++ "," ++ show y ++ ", " ++ show tag ++ ")"
  show (GammaDist a b y tag) =
    "GammaDist(" ++ show a ++ ", " ++ show b ++ "," ++ show y ++ ", " ++ show tag ++ ")"
  show (UniformDist a b y tag) =
    "UniformDist(" ++ show a ++ ", " ++ show b ++ "," ++ show y ++ ", " ++ show tag ++ ")"
  show (DiscrUniformDist min max y tag) =
    "DiscrUniformDist(" ++ show min ++ ", " ++ show max ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (PoissonDist l y tag) =
    "PoissonDist(" ++ show l ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (CategoricalDist xs y tag) =
    "CategoricalDist(" ++ show xs ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

type Tag  = String
type Addr = (Tag, Int)

data Sample a where
  Sample  :: Dist a -> Addr -> Sample a
  Printer :: String -> Sample ()

prinT :: Member Sample es => String -> Freer es ()
prinT s = Free (inj $ Printer s) Pure

data Observe a where
  Observe :: Dist a -> a -> Addr -> Observe a

pattern Print :: Member Sample rs => String -> Union rs ()
pattern Print s <- (prj -> Just (Printer s))

pattern Samp :: Member Sample rs => Dist x -> Addr -> Union rs x
pattern Samp d α <- (prj -> Just (Sample d α))

pattern Obs :: Member Observe rs => Dist x -> x -> Addr -> Union rs x
pattern Obs d y α <- (prj -> Just (Observe d y α))

pattern DistDouble :: Maybe (Dist Double) -> Dist x
pattern DistDouble d <- (isDistDouble -> d)
pattern DistBool :: Maybe (Dist Bool) -> Dist x
pattern DistBool d <- (isDistBool -> d)
pattern DistInt :: Maybe (Dist Int) -> Dist x
pattern DistInt d <- (isDistInt -> d)

isDistDouble :: Dist x -> Maybe (Dist Double)
isDistDouble d@NormalDist {} = Just d
isDistDouble d@BetaDist {} = Just d
isDistDouble d@GammaDist {} = Just d
isDistDouble d@UniformDist {} = Just d
isDistDouble d@DiscrUniformDist {} = Nothing
isDistDouble d@BinomialDist {} = Nothing
isDistDouble d@CategoricalDist {} = Nothing
isDistDouble d@DiscreteDist {} = Nothing
isDistDouble d@BernoulliDist {} = Nothing
isDistDouble d@PoissonDist {} = Nothing

isDistBool :: Dist x -> Maybe (Dist Bool)
isDistBool d@BernoulliDist {} = Just d
isDistBool d@NormalDist {} = Nothing
isDistBool d@BetaDist {} = Nothing
isDistBool d@GammaDist {} = Nothing
isDistBool d@UniformDist {} = Nothing
isDistBool d@DiscrUniformDist {} = Nothing
isDistBool d@BinomialDist {} = Nothing
isDistBool d@CategoricalDist {} = Nothing
isDistBool d@DiscreteDist {} = Nothing
isDistBool d@PoissonDist {} = Nothing

isDistInt :: Dist x -> Maybe (Dist Int)
isDistInt d@DiscrUniformDist {} = Just d
isDistInt d@BinomialDist {} = Just d
isDistInt d@CategoricalDist {} = Just d
isDistInt d@DiscreteDist {} = Just d
isDistInt d@PoissonDist {} = Just d
isDistInt d@BernoulliDist {} = Nothing
isDistInt d@NormalDist {} = Nothing
isDistInt d@BetaDist {} = Nothing
isDistInt d@GammaDist {} = Nothing
isDistInt d@UniformDist {} = Nothing

type TagMap = Map Tag Int

{- Replaces Dists with Sample or Observe and adds address -}
runDist :: forall rs a. (Member Sample rs, Member Observe rs) => Freer (Dist : rs) a
        -> Freer rs a
runDist = loop 0 Map.empty
  where
  loop :: (Member Sample rs, Member Observe rs) => Int -> TagMap -> Freer (Dist : rs) a -> Freer rs a
  loop α_counter tagMap (Pure x) = return x
  loop α_counter tagMap (Free u k) = case decomp u of
    Right d
      -> let tag     = if hasTag d then getTag d else show α_counter
             tag_i   = Map.findWithDefault 0 tag tagMap
             tagMap' = Map.insert tag (tag_i + 1) tagMap
         in if hasObs d
            then send (Observe d (getObs d) (tag, tag_i)) >>= loop (α_counter + 1) tagMap' . k
            else send (Sample d (tag, tag_i)) >>= loop (α_counter + 1) tagMap' . k
    Left  u'  -> Free u' (loop α_counter tagMap. k)

{- Replaces Dists with Sample or Observe and adds address -}
-- runDist :: forall rs a. (Member Sample rs, Member Observe rs) => Freer (Dist : rs) a
--         -> Freer rs a
-- runDist = loop 0
--   where
--   loop :: (Member Sample rs, Member Observe rs) => Int -> Freer (Dist : rs) a -> Freer rs a
--   loop α_counter (Pure x) = return x
--   loop α_counter (Free u k) = case decomp u of
--     Right d
--       -> let α = if hasTag d then getTag d else show α_counter
--          in if hasObs d
--             then send (Observe d (getObs d) α) >>= loop (α_counter + 1) . k
--             else send (Sample d α) >>= loop (α_counter + 1). k
--     Left  u'  -> Free u' (loop α_counter . k)


instance Distribution (Dist a) where
  cumulative (NormalDist μ σ _ _) x
    = cumulative (normalDistr μ σ) x
  cumulative (UniformDist min max _ _) x
    = cumulative (uniformDistr min max) x
  cumulative (GammaDist k θ _ _) x
    = cumulative (gammaDistr k θ) x
  cumulative (BetaDist α β _ _) x
    = cumulative (betaDistr α β) x
  cumulative (BinomialDist n p _ _) x
    = cumulative (binomial n p) x
  cumulative (BernoulliDist p _ _) x
    = cumulative (binomial 1 p) x
  cumulative (CategoricalDist ps _ _) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps
  cumulative (DiscreteDist ps _ _) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps
  cumulative (PoissonDist λ _ _) x
    = cumulative (poisson λ) x
  cumulative (DiscrUniformDist min max _ _) x
    = cumulative (uniformDistr (fromIntegral min) (fromIntegral max)) x

instance ContDistr (Dist a) where
  density (NormalDist μ σ _ _)          = density (normalDistr μ σ)
  density (UniformDist min max _ _)     = density (uniformDistr min max)
  density (GammaDist k θ _ _)           = density (gammaDistr k θ)
  density (BetaDist α β _ _)            = density (betaDistr α β)
  logDensity (NormalDist μ σ _ _)       = logDensity (normalDistr μ σ)
  logDensity (UniformDist min max _ _)  = logDensity (uniformDistr min max)
  logDensity (GammaDist k θ _ _)        = logDensity (gammaDistr k θ)
  logDensity (BetaDist α β _ _)         = logDensity (betaDistr α β)
  quantile (NormalDist μ σ _ _)         = quantile (normalDistr μ σ)
  quantile (UniformDist min max _ _)    = quantile (uniformDistr min max)
  quantile (GammaDist k θ _ _)          = quantile (gammaDistr k θ)
  quantile (BetaDist α β _ _)           = quantile (betaDistr α β)

instance DiscreteDistr (Dist a) where
  -- binomial: probability of `i` successful outcomes
  probability (BinomialDist n p _ _) i            = probability (binomial n p) i
  probability (BernoulliDist p _ _) i             = probability (binomial 1 p) i
  probability (CategoricalDist ps _ _) i          = snd (ps V.! i)
  probability (DiscreteDist ps _ _) i             = snd (ps !! i)
  probability (DiscrUniformDist min max _ _) i    = probability (discreteUniformAB min max) i
  probability (PoissonDist λ _ _) i               = probability (poisson λ) i
  logProbability (BinomialDist n p _ _) i         = logProbability (binomial n p) i
  logProbability (BernoulliDist p _ _) i          = logProbability (binomial 1 p) i
  logProbability (CategoricalDist ps _ _) i       = (log . snd) (ps V.! i)
  logProbability (DiscreteDist ps _ _) i          = (log . snd) (ps !! i)
  logProbability (DiscrUniformDist min max _ _) i = logProbability (discreteUniformAB min max) i
  logProbability (PoissonDist λ _ _) i = logProbability (poisson λ) i

hasObs :: Dist a -> Bool
hasObs d@(NormalDist _ _ obs _)       = isJust obs
hasObs d@(DiscrUniformDist _ _ obs _) = isJust obs
hasObs d@(UniformDist _ _ obs _)      = isJust obs
hasObs d@(GammaDist _ _ obs _)        = isJust obs
hasObs d@(BetaDist _ _ obs _)         = isJust obs
hasObs d@(BinomialDist _ _ obs _)     = isJust obs
hasObs d@(BernoulliDist _ obs _)      = isJust obs
hasObs d@(CategoricalDist _ obs _)    = isJust obs
hasObs d@(DiscreteDist _ obs _)       = isJust obs
hasObs d@(PoissonDist _ obs _)        = isJust obs

hasTag :: Dist a -> Bool
hasTag d@(NormalDist _ _ _ tag)       = isJust tag
hasTag d@(DiscrUniformDist _ _ _ tag) = isJust tag
hasTag d@(UniformDist _ _ _ tag)      = isJust tag
hasTag d@(GammaDist _ _ _ tag)        = isJust tag
hasTag d@(BetaDist _ _ _ tag)         = isJust tag
hasTag d@(BinomialDist _ _ _ tag)     = isJust tag
hasTag d@(BernoulliDist _ _ tag)      = isJust tag
hasTag d@(CategoricalDist _ _ tag)    = isJust tag
hasTag d@(DiscreteDist _ _ tag)       = isJust tag
hasTag d@(PoissonDist _ _ tag)        = isJust tag

getObs :: Dist a -> a
getObs d@(NormalDist _ _ obs _)       = fromJust obs
getObs d@(DiscrUniformDist _ _ obs _) = fromJust obs
getObs d@(UniformDist _ _ obs _)      = fromJust obs
getObs d@(GammaDist _ _ obs _)        = fromJust obs
getObs d@(BetaDist _ _ obs _)         = fromJust obs
getObs d@(BinomialDist _ _ obs _)     = fromJust obs
getObs d@(BernoulliDist _ obs _)      = fromJust obs
getObs d@(CategoricalDist _ obs _)    = fromJust obs
getObs d@(DiscreteDist _ obs _)       = fromJust obs
getObs d@(PoissonDist _ obs _)        = fromJust obs

getTag :: Dist a -> String
getTag d@(NormalDist _ _ _ tag)       = fromJust tag
getTag d@(DiscrUniformDist _ _ _ tag) = fromJust tag
getTag d@(UniformDist _ _ _ tag)      = fromJust tag
getTag d@(GammaDist _ _ _ tag)        = fromJust tag
getTag d@(BetaDist _ _ _ tag)         = fromJust tag
getTag d@(BinomialDist _ _ _ tag)     = fromJust tag
getTag d@(BernoulliDist _ _ tag)      = fromJust tag
getTag d@(CategoricalDist _ _ tag)    = fromJust tag
getTag d@(DiscreteDist _ _ tag)       = fromJust tag
getTag d@(PoissonDist _ _ tag)        = fromJust tag


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
prob d@PoissonDist {} y
  = probability d y

{- Combines logDensity and logProbability.
   Log probability of double `x` from a distribution -}
logProb :: Dist a -> a -> Double
logProb d = log . prob d

sample :: Dist a -> Sampler a
sample (NormalDist μ σ obs _)  =
  createSampler (sampleNormal μ σ)
sample (UniformDist min max obs  _)  =
  createSampler (sampleUniform min max)
sample (DiscrUniformDist min max obs _)  =
  createSampler (sampleDiscreteUniform min max)
sample (GammaDist k θ obs _)        =
  createSampler (sampleGamma k θ)
sample (BetaDist α β  obs _)         =
  createSampler (sampleBeta α β)
sample (BinomialDist n p  obs _)     =
  createSampler (sampleBinomial n p) >>=  return .  length . filter (== True)
sample (BernoulliDist p obs _)      =
  createSampler (sampleBernoulli p)
sample (CategoricalDist ps obs _)   =
  createSampler (sampleCategorical (fmap snd ps))
sample (DiscreteDist ps obs _)      =
  createSampler (sampleDiscrete (map snd ps)) >>= \i -> return (fst $ ps !! i)
sample (PoissonDist λ obs _) =
  createSampler (samplePoisson λ)
