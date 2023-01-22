
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.VI.VI
  where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>), mapAndUnzipM )
import Effects.Dist
import Model
import Effects.Lift
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import Inference.MC.LW (joint)

type VIGuide env es a  = Prog (EnvRW env : Param : Sample : es) a
type VIModel env es a  = Prog (EnvRW env : Observe : Sample : es) a

data GradDescent a where
  GradDescent :: [LogP] -> [GradTrace] -> ParamTrace -> GradDescent ParamTrace

type GuideHandler env es a = VIGuide env es a -> ParamTrace -> Sampler (((a, Env env), LogP), GradTrace)
type ModelHandler env es a = VIModel env es a -> Env env    -> Sampler (a, LogP)

viLoop :: (Members [GradDescent, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es1 a -> GuideHandler env es1 a
  -> VIModel env es2 b -> ModelHandler env es2 b
  -> ParamTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Prog fs ParamTrace      -- ^ final guide parameters λ_T
viLoop num_timesteps num_samples guide hdlGuide model hdlModel guideParams_0 = do
  foldr (>=>) pure [viStep num_samples  hdlGuide  hdlModel guide model  | t <- [1 .. num_timesteps]] guideParams_0

{- | 1. For L iterations,
        a) Generate values x from the guide Q(X; λ), accumulating:
            - the total guide log-weight:  log(Q(X = x; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
        b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
            - the total model log-weight:  log(P(X=x, Y=y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the parameters λ of the guide
-}

viStep ::  (Members [GradDescent, Sampler] fs)
  => Int
  -> GuideHandler env es1 a -> ModelHandler env es2 b -> VIGuide env es1 a -> VIModel env es2 b
  -> ParamTrace                            -- ^ guide parameters λ_t
  -> Prog fs ParamTrace    -- ^ next guide parameters λ_{t+1}
viStep num_samples hdlGuide hdlModel guide model  params = do
  -- | Execute the guide X ~ Q(X; λ) for (L) iterations
  (((_, envs), guide_ρs), grads) <- Util.unzip4 <$> replicateM num_samples (call (hdlGuide guide params))
  -- | Execute the model P(X, Y) under the union of the model environment Y and guide environment X
  (_              , model_ρs)  <- mapAndUnzipM (call . hdlModel model) envs
  -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
  let ρs      = zipWith (-) model_ρs guide_ρs
  -- | Update the parameters λ of the proposal distributions Q
  params'  <- call (GradDescent ρs grads params)
  -- liftPutStrLn (show modelParams')
  pure params'

{- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
      1. The output environment of latent variables X=x (given `env` contains X) generated by @Sample@s
         i.e. fixed non-differentiable dists, and @Param@s, i.e. learnable proposal dists:
            x ~ Q(X; λ),          where Q can be fixed or learnable
      2. The total log-weight of latent variables X=x, resulting from @Sample@s and @Param@s:
            log(Q(X=x; λ)),      where Q can be fixed or learnable
      3. The gradients of all proposal distributions Q(λ) at X=x:
            δlog(Q(X=x; λ)),     where Q is learnable
 -}


-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectParams :: es ~ '[Sampler] => Env env -> VIGuide env es a -> Sampler ParamTrace
collectParams env = handleM . SIM.defaultSample . (fst <$>) . handleParams . loop Trace.empty . handleEnvRW env
  where
  loop :: ParamTrace -> Prog (Param : es) a -> Prog (Param : es) ParamTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Param q α)   -> do let params' = Trace.insert (Key α) q params
                             Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

-- | Set the @Param@eters of the guide Q(X; λ).
updateParams :: forall es a. Member Param es => ParamTrace -> Prog es a -> Prog es a
updateParams proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val a)   = pure a
  loop (Op op k) = case prj op of
    Just (Param q α) -> do
      let q' = fromMaybe q (Trace.lookup (Key α) proposals)
      x <- call (Param q' α)
      (loop . k) x
    Nothing -> Op op (loop . k)

-- | Compute log(Q(X; λ)) over the guide.
weighGuide :: forall es a. (Members [Param, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      ParamPrj q α   -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj q α     -> Op op (\x -> loop (logW + logProb q x) $ k x)
      _               -> Op op (loop logW . k)

-- | Sample from each @Param@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
handleParams :: forall es a. Member Sample es => Prog (Param : es) a -> Prog es (a, GradTrace)
handleParams = loop Trace.empty where
  loop :: GradTrace -> Prog (Param : es) a -> Prog es (a, GradTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (Param (q :: d) α) -> do
      x <- call (Sample q α)
      let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
      (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
gradStep
  :: Double  -- ^ learning rate             η
  -> ParamTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> ParamTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
