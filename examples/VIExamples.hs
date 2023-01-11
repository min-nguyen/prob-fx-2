

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# LANGUAGE TypeApplications #-}

{- | A linear regression model, modelling a linear relationship between data points x and y.
-}

module VIExamples where

import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.SMC2 as SMC2 ( smc2 )
import Inference.VI.Icfp23.VI
import qualified Inference.VI.Icfp23.BBVI as BBVI
import Sampler ( Sampler, sampleIO, liftIO, sampleIOFixed )
import qualified Trace
import           Trace (Key(..))
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Effects.Dist
import PrimDist
import Data.Maybe
import Prog


{- | Linear regression environment.
-}
type LinRegrEnv =
    '[  "m" ':= Double, -- ^ gradient
        "c" ':= Double, -- ^ intercept
        "σ" ':= Double, -- ^ noise
        "y" ':= Double    -- ^ output
     ]

{- | Linear regression as a probabilistic program for inference -}
linRegr :: forall env. Observables env '["m", "c"] Double
  => [(Double, Double)] -> VIModel env (Double, Double)  -- ^ y datapoints
linRegr xys = do
  -- Draw model parameters from prior
  m <- sample' @env (mkNormal 0 3) (#m, 0)
  c <- sample' @env (mkNormal 0 5) (#c, 0)
  -- Generate outputs ys
  mapM_ (\((x, y), idx) -> observe (mkNormal (m * x + c) 1) y (Addr "y" idx)) (zip xys [0 ..])
  return (m, c)

linRegrGuide :: forall env. Observables env '["m", "c"] Double
  => VIGuide env ()
linRegrGuide = do
  m <- param' @env (mkNormal 0 3) (#m, 0)
  c <- param' @env (mkNormal 0 5) (#c, 0)
  return ()

bbviLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
bbviLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- BBVI.bbvi t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)
