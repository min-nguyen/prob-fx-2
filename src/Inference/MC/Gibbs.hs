
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Inference.MC.Gibbs where

import Inference.MC.Metropolis

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember )
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP, expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Effects.Dist ( Dist, Addr, Tag )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import Inference.MC.Metropolis as Metropolis


{- Propose a new random value at a single component x_i of latent variable X = {x_0, ... x_N}.
-}
propose
  :: [Tag]                          -- ^ observable variable names of interest
  -> STrace                         -- ^ original sample trace
  -> Sampler ([Addr], STrace)       -- ^ (proposed addresses, proposed sample trace)
propose tags strace = do
  -- | Get possible addresses to propose new samples for
  let α_range = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
  -- | Draw proposal sample addresse
  α <- sample (mkUniformD 0 (length α_range - 1)) <&> (α_range !!)
  -- | Draw new random value
  r <- sampleRandom
  let strace' = Map.insert α r strace
  return ([α], strace')

handleAccept :: LastMember (Lift Sampler) fs
  => Prog (Accept (Int, LogP) : fs) a
  -> Prog fs a
handleAccept (Val x)    = pure x
handleAccept (Op op k) = case discharge op of
    Right (Propose strace (α_idx, logp))
      ->  do r <- lift sampleRandom
             let strace' = Map.updateAt (\_ _ -> Just r) (α_idx + 1) strace
             (handleAccept . k) (α_idx, strace')
    Right (Accept αs (α, logp) (α', logp'))
      ->  do u <- lift $ sample (mkUniform 0 1)
             (handleAccept . k) (expLogP (logp' - logp) > u)
    Left op' -> Op op' (handleAccept . k)