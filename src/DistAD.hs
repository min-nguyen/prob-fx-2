
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module DistAD where

import Data.Map
import Comp
import Sampler
import Numeric.MathFunctions.Constants
import AD
import Control.Monad

data Beta d = Beta d d
data Normal d = Normal d d

data Sample d a where
  Sample  :: Normal d -> Sample d d

data Observe d a where
  Observe :: Normal d -> d -> Observe d d

type DiffModel d a = Comp '[Observe d] a

gradAsc :: Int -> (Nagata String Double -> DiffModel (Nagata String Double) a) -> Nagata String Double
gradAsc steps model =
  let p = N 0.5 (singleton "m" 1)
      go p@(N x dx) i
             | i > steps = p
             | otherwise = let N x' dx' = runPure $ likelihood (model p)
                           in  go (N (x + 0.001 * (dx' ! "m")) dx) (i + 1)
  in  go p 0

example_xys :: Fractional d => [(d, d)]
example_xys = [(1,1.2),(2,1.8),(3,3.1),(4,4.2),(5, 5.0)]

example :: Num d => [(d, d)] -> d -> DiffModel d ()
example xys m = do
  forM xys (\(x, y) -> call $ Observe (Normal (m * x) 1) y)
  return ()

-- | Handle @Observe@ operations by simply passing forward their observed value, performing no side-effects
-- defaultObserve :: LogProb d => Handler (Observe d) es b b
-- defaultObserve = handleWith () (const Val) (const hop) where
--   hop :: LogProb d => Observe d x -> (() -> x -> Comp es b) -> Comp es b
--   hop (Observe d x ) k =  k () (logProb d x)
likelihood :: LogProb d => Handler (Observe d) es a d
likelihood  = handleWith 0 (\lρ x -> Val lρ) hop
  where
  hop :: LogProb d => d -> Observe d x -> (d -> x -> Comp es d) -> Comp es d
  hop lρ (Observe d y) k = k (lρ + logProb d y) y


class (Num d) => LogProb d where
  logProb :: Normal d -> d -> d

instance LogProb Double where
  logProb (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

instance LogProb (Nagata String Double) where
  logProb (Normal (N μ dμ) (N σ dσ)) (N x dx)
    = N (logProb (Normal μ σ) x)
        (unionWith (+) (fmap (dpμ *) dμ) (fmap (dpσ *) dσ))
    where xμ = x - μ
          dpμ = xμ/(σ ** 2)
          dpσ = -1/σ + (xμ**2)/(σ ** 3)
-- gradLogProb :: Normal -> Double -> Vec Nat2 Double
-- gradLogProb (Normal μ σ) x = dpμ ::: dpσ ::: VNil
--   where xμ = x - μ
--         dpμ = xμ/(σ ** 2)
--         dpσ = -1/σ + (xμ**2)/(σ ** 3)


-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
defaultSample ::  Member Sampler es => Handler (Sample Double) es a a
defaultSample = handleWith () (const Val) (const hop) where
  hop :: Member Sampler es => Sample Double x -> (() -> x -> Comp es b) -> Comp es b
  hop (Sample (Normal mu s :: Normal Double ) ) k = do x <- call $ normal mu s
                                                       k () x

