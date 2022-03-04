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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
module Extensible.Dist where

import Extensible.Freer
    ( Prog(..), Member(..), EffectSum, decomp, send )
import Extensible.Sampler
import Extensible.OpenSum (OpenSum)
import qualified Extensible.OpenSum as OpenSum
import Util ( boolToInt )
import Control.Lens hiding ((:>))
import Control.Monad.State
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Statistics.Distribution
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Poisson
import Statistics.Distribution.Normal
import Statistics.Distribution.Dirichlet
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.Uniform
import Numeric.Log
import qualified System.Random.MWC.Distributions as MWC
import Data.GADT.Compare (GEq)

-- data Forall where
--   F :: (forall a. Show a => a) -> Forall


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
  HalfNormalDist  {} -> Dict
  UniformDist  {} -> Dict
  DiscrUniformDist {} -> Dict
  GammaDist {} -> Dict
  BetaDist {} -> Dict
  BinomialDist {} -> Dict
  BernoulliDist {} -> Dict
  CategoricalDist {} -> Dict
  DiscreteDist {} -> Dict
  PoissonDist {} -> Dict
  DirichletDist {} -> Dict
  DeterministicDist {} -> Dict

instance Show PrimDist where
  show (PrimDist d) = show d

data Dist a where
  HalfCauchyDist    :: Double -> Maybe Double -> Maybe String -> Dist Double
  CauchyDist        :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  NormalDist        :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  HalfNormalDist    :: Double -> Maybe Double -> Maybe String -> Dist Double
  UniformDist       :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> Maybe String -> Dist Int
  GammaDist         :: Double -> Double -> Maybe Double -> Maybe String -> Dist Double
  BetaDist          :: Double -> Double -> Maybe Double -> Maybe String ->  Dist Double
  BinomialDist      :: Int    -> Double -> Maybe Int -> Maybe String -> Dist Int
  BernoulliDist     :: Double -> Maybe Bool -> Maybe String -> Dist Bool
  CategoricalDist   :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Maybe a -> Maybe String -> Dist a
  DiscreteDist      :: [Double] -> Maybe Int -> Maybe String -> Dist Int
  PoissonDist       :: Double -> Maybe Int -> Maybe String -> Dist Int
  DirichletDist     :: [Double] -> Maybe [Double] -> Maybe String -> Dist [Double]
  DeterministicDist :: (Eq a, Show a, OpenSum.Member a PrimVal) => a -> Maybe a -> Maybe String -> Dist a

instance Eq (Dist a) where
  (==) (NormalDist m s _ _) (NormalDist m' s' _ _) = m == m' && s == s'
  (==) (CauchyDist m s _ _) (CauchyDist m' s' _ _) = m == m' && s == s'
  (==) (HalfCauchyDist s _ _) (HalfCauchyDist s' _ _) = s == s'
  (==) (HalfNormalDist s _ _) (HalfNormalDist s' _ _) = s == s'
  (==) (BernoulliDist p _ _) (BernoulliDist p' _ _) = p == p'
  (==) (BinomialDist n p _ _) (BinomialDist n' p' _ _) = n == n' && p == p'
  (==) (DiscreteDist ps _ _) (DiscreteDist ps' _ _) = ps == ps'
  (==) (BetaDist a b _ _) (BetaDist a' b' _ _) = a == a' && b == b'
  (==) (GammaDist a b _ _) (GammaDist a' b' _ _) = a == a' && b == b'
  (==) (UniformDist a b _ _) (UniformDist a' b' _ _) = a == a' && b == b'
  (==) (DiscrUniformDist min max _ _) (DiscrUniformDist min' max' _ _) = min == min' && max == max'
  (==) (PoissonDist l _ _) (PoissonDist l' _ _) = l == l'
  (==) (CategoricalDist xs _ _) (CategoricalDist xs' _ _) = xs == xs'
  (==) (DirichletDist xs _ _) (DirichletDist xs' _ _)  = xs == xs'
  (==) (DeterministicDist x _ _) (DeterministicDist x' _ _) = x == x'
  (==) _ _ = False

instance Show a => Show (Dist a) where
  show (CauchyDist mu sigma y tag) =
   "CauchyDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (HalfCauchyDist sigma y tag) =
   "HalfCauchyDist(" ++ show sigma ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (NormalDist mu sigma y tag) =
   "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (HalfNormalDist sigma y tag) =
   "HalfNormalDist(" ++ show sigma ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
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
  show (DirichletDist xs y tag) =
   "DirichletDist(" ++ show xs ++ ", " ++ show y ++ ", " ++ show tag ++ ")"
  show (DeterministicDist x y tag) =
   "DeterministicDist(" ++ show x ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

type Tag  = String
type Addr = (Tag, Int)

type TagMap = Map Tag Int

{- Replaces Dists with Sample or Observe and adds address -}
runDist :: forall es a. (Member Sample es, Member Observe es)
        => Prog (Dist : es) a -> Prog es a
runDist = loop 0 Map.empty
  where
  loop :: (Member Sample es, Member Observe es)
       => Int -> TagMap -> Prog (Dist : es) a -> Prog es a
  loop _ _ (Val x) = return x
  loop counter tagMap (Op u k) = case decomp u of
    Right d ->
         case getObs d of
              Just y  -> do send (Observe d y (tag, tagIdx)) >>= k'
              Nothing -> do send (Sample d (tag, tagIdx))    >>= k'
          where tag     = fromMaybe (show counter) (getTag d)
                tagIdx  = Map.findWithDefault 0 tag tagMap
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop (counter + 1) tagMap' . k
    Left  u'  -> Op u' (loop counter tagMap . k)

-- runDist :: forall es a. Prog (Dist ': es) a -> Prog (Observe ': Sample ': es) a
-- runDist = replaceRelayStN @'[Observe, Sample] (0, Map.empty) (\_ x -> return x)
--   undefined
--  (forall x. s -> t x -> (s -> x -> Prog (es :++: ts) b) -> Prog (es :++: ts) b) (Prog (t : ts) a)

data Sample a where
  Sample  :: Dist a -> Addr -> Sample a
  Printer :: String -> Sample ()

prinT :: Member Sample es => String -> Prog es ()
prinT s = Op (inj $ Printer s) Val

data Observe a where
  Observe :: Dist a -> a -> Addr -> Observe a

pattern PrintPatt :: (Member Sample es) => (x ~ ()) => String -> EffectSum es x
pattern PrintPatt s <- (prj -> Just (Printer s))

pattern DistPatt :: () => (Show x, OpenSum.Member x PrimVal) => Dist x -> EffectSum (Dist : r) x
pattern DistPatt d <- (decomp -> Right (DistDict d))

pattern DistDict :: () => (Show x, OpenSum.Member x PrimVal) => Dist x -> Dist x
pattern DistDict d <- d@(distDict -> Dict)

pattern Samp :: Member Sample es => Dist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj -> Just (Sample d α))

pattern SampPatt :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => Dist x -> Addr -> EffectSum es x
pattern SampPatt d α <- (Samp (DistDict d) α)

pattern SampPatt' :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => Dist x -> Addr -> EffectSum es x
pattern SampPatt' d α <- (prj -> Just (Sample d@(distDict -> Dict) α))

isExprInt :: Dist x -> Maybe (Int :~: x)
isExprInt e@(BinomialDist _ _ _ _) = Just Refl
isExprInt _         = Nothing

pattern DistInt :: () => x ~ Int => Dist x
pattern DistInt  <- (isExprInt -> Just Refl)

pattern ExprIntPrj :: Member Dist es => x ~ Int => Dist x -> EffectSum es x
pattern ExprIntPrj e <- (prj -> Just e@DistInt)

pattern Obs :: Member Observe es => Dist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

pattern ObsPatt :: (Member Observe es) => (Show x, OpenSum.Member x PrimVal) => Dist x -> x -> Addr -> EffectSum es x
pattern ObsPatt d y α <- (Obs (DistDict d) y α)

pattern DecompLeft :: EffectSum es a -> EffectSum ((:) e es) a
pattern DecompLeft u <- (decomp -> Left u)

pattern DecompRight :: e a -> EffectSum (e : es) a
pattern DecompRight u <- (decomp -> Right u)

getObs :: Dist a -> Maybe a
getObs d@(HalfCauchyDist _ obs _)     =  obs
getObs d@(CauchyDist _ _ obs _)       =  obs
getObs d@(HalfNormalDist _ obs _)     =  obs
getObs d@(NormalDist _ _ obs _)       =  obs
getObs d@(DiscrUniformDist _ _ obs _) =  obs
getObs d@(UniformDist _ _ obs _)      =  obs
getObs d@(GammaDist _ _ obs _)        =  obs
getObs d@(BetaDist _ _ obs _)         =  obs
getObs d@(BinomialDist _ _ obs _)     =  obs
getObs d@(BernoulliDist _ obs _)      =  obs
getObs d@(CategoricalDist _ obs _)    =  obs
getObs d@(DiscreteDist _ obs _)       =  obs
getObs d@(PoissonDist _ obs _)        =  obs
getObs d@(DirichletDist _ obs _)      =  obs
getObs d@(DeterministicDist _ obs _)  =  obs

getTag :: Dist a -> Maybe String
getTag d@(HalfCauchyDist _ _ tag)     = tag
getTag d@(CauchyDist _ _ _ tag)       = tag
getTag d@(HalfNormalDist _ _ tag)     = tag
getTag d@(NormalDist _ _ _ tag)       = tag
getTag d@(DiscrUniformDist _ _ _ tag) = tag
getTag d@(UniformDist _ _ _ tag)      = tag
getTag d@(GammaDist _ _ _ tag)        = tag
getTag d@(BetaDist _ _ _ tag)         = tag
getTag d@(BinomialDist _ _ _ tag)     = tag
getTag d@(BernoulliDist _ _ tag)      = tag
getTag d@(CategoricalDist _ _ tag)    = tag
getTag d@(DiscreteDist _ _ tag)       = tag
getTag d@(PoissonDist _ _ tag)        = tag
getTag d@(DirichletDist _ _ tag)      = tag
getTag d@(DeterministicDist _ _ tag)  = tag

prob :: Dist a -> a -> Double
prob (DirichletDist xs _ _) ys =
  let xs' = map (/(Prelude.sum xs)) xs
  in  if Prelude.sum xs' /= 1 then error "dirichlet can't normalize" else
      case dirichletDistribution (UV.fromList xs')
      of Left e -> error "dirichlet error"
         Right d -> let Exp p = dirichletDensity d (UV.fromList ys)
                        in  exp p
prob (HalfCauchyDist σ _ _) y
  = if y < 0 then 0 else
            2 * density (cauchyDistribution 0 σ) y
prob (CauchyDist μ σ _ _) y
  = density (cauchyDistribution μ σ) y
prob (HalfNormalDist σ _ _) y
  = if y < 0 then 0 else
            2 * density (normalDistr 0 σ) y
prob (NormalDist μ σ _ _) y
  = density (normalDistr μ σ) y
prob (UniformDist min max _ _) y
  = density (uniformDistr min max) y
prob (GammaDist k θ _ _) y
  = density (gammaDistr k θ) y
prob  (BetaDist α β _ _) y
  = density (betaDistr α β) y
prob (DiscrUniformDist min max _ _) y
  = probability (discreteUniformAB min max) y
prob (BinomialDist n p _ _) y
  = probability (binomial n p) y
prob (BernoulliDist p _ _) i
  = probability (binomial 1 p) (boolToInt i)
prob d@(CategoricalDist ps _ _) y
  = case lookup y ps of
      Nothing -> error $ "Couldn't find " ++ show y ++ " in categorical dist"
      Just p  -> p
prob (DiscreteDist ps _ _) y
  = ps !! y
prob (PoissonDist λ _ _) y
  = probability (poisson λ) y
prob (DeterministicDist x _ _) y
  = 1

{- Combines logDensity and logProbability.
   Log probability of double `x` from a distribution -}
logProb :: Dist a -> a -> Double
logProb d = log . prob d

sample :: Dist a -> Sampler a
sample (HalfCauchyDist σ obs _)  =
  createSampler (sampleCauchy 0 σ) >>= return . abs
sample (CauchyDist μ σ obs _)  =
  createSampler (sampleCauchy μ σ)
sample (HalfNormalDist σ obs _)  =
  createSampler (sampleNormal 0 σ) >>= return . abs
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
  createSampler (sampleCategorical (V.fromList $ fmap snd ps)) >>= \i -> return $ fst $ ps !! i
sample (DiscreteDist ps obs _)      =
  createSampler (sampleDiscrete ps)
sample (PoissonDist λ obs _) =
  createSampler (samplePoisson λ)
sample (DirichletDist xs _ _) =
  createSampler (sampleDirichlet xs)
sample (DeterministicDist x _ _) = return x
