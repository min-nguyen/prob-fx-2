{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe, Sample )
import Effects.Lift ( Lift, lift )
import Effects.NonDet ( asum, handleNonDet, NonDet )
import Effects.ObsRW
import Effects.Resample (Resample(..))
import Env ( Env )
import LogP ( LogP(..), logMeanExp )
import Model ( Model(runModel) )
import OpenSum (OpenSum)
import PrimDist ( PrimDist(Categorical), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Sampler ( Sampler )

{- | The particle context for SMC
-}
data SMCParticle = SMCParticle {
    particleLogProb  :: LogP    -- ^ associated log-probability
  , particleObsAddrs :: [Addr]  -- ^ addresses of @Observe@ operations encountered so far
  }

instance SIS.ParticleCtx SMCParticle where
  pempty            = SMCParticle 0 []
  paccum ctxs ctxs' =
    --  Compute normalised accumulated log weights
    let logprobs = let logZ = logMeanExp (map particleLogProb ctxs)
                   in  map ((+ logZ) . particleLogProb) ctxs'
    --  Update the Observe operations encountered
        obsaddrs = zipWith (++) (map particleObsAddrs ctxs') (map particleObsAddrs ctxs)
    in  zipWith SMCParticle logprobs obsaddrs

{- | A top-level function for calling Sequential Monte Carlo on a model.
-}
smc :: Show a
  => Int
  -> Model env [ObsRW env, Dist, Lift Sampler] a
  -> Env env
  -> Sampler [Env env]
smc n_particles model env = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  final_ctxs <- smcInternal n_particles prog env
  pure $ map (snd . fst) final_ctxs

{- | For calling Sequential Monte Carlo on a probabilistic program.
-}
smcInternal
  :: Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Env env
  -> Sampler [(a, SMCParticle)]
smcInternal n_particles prog env =
  SIS.sis n_particles smcParticleHdlr handleResample SIM.handleSamp (weakenProg prog)

{- | Runs a population of particles to the next breakpoint in the program.

Maybe toy with an idea like:

type ParticleHandler ctx es a
  =  Prog (NonDet : es) a
  -> Prog (Resample : es) a

data Resample es ctx a where
  Resample :: [(Prog (NonDet : es) a, ctx)] -> Resample es ctx [(Prog (NonDet : es) a, ctx)]
-}

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
       3. the address of the breakpoint
-}

handleResample :: (LastMember (Lift Sampler) es) => Prog (Resample SMCParticle : es) a -> Prog es a
handleResample (Val x) = Val x
handleResample (Op op k) = case discharge op of
  Left  op'               -> Op op' (handleResample  . k)
  Right (Resample (vals, ctxs)) -> do
    -- Accumulate the contexts of all particles and get their normalised log-weights
    let logws      = map (exp . unLogP . particleLogProb) ctxs
    -- Select particles to continue with
    idxs <- replicateM (length ctxs) $ lift (sample (Categorical logws))
    let resampled_vals = map (vals !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs
    (handleResample . k) (resampled_vals, resampled_ctxs)

smcParticleHdlr :: Member Observe es
  => Prog es a
  -> Prog es (Prog es a, SMCParticle)
smcParticleHdlr  (Val x) = pure (Val x, SMCParticle 0 [("", 0)])
smcParticleHdlr  (Op op k) = case op of
      ObsPrj d y α -> Val (k y, SMCParticle (logProb d y) [α])
      _            -> Op op (smcParticleHdlr . k)

