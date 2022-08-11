{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Resample where

import Control.Monad
import Prog
import Sampler
import PrimDist
import Effects.Lift
import LogP
-- import Inference.SIS

data Resample ctx a where
  Resample :: [ctx] -> Resample ctx [Int]

particleLogProb = undefined

handleResample :: (LastMember (Lift Sampler) es) => [ctx] -> Prog (Resample ctx : es) a -> Prog es (Prog es a)
handleResample _ (Val x) = pure (Val x)
handleResample ctxs_0 (Op op k) = case discharge op of
  Left  op'               -> Op op' (handleResample ctxs_0 . k)
  Right (Resample ctxs_1) -> do
    -- Accumulate the contexts of all particles and get their normalised log-weights
    let ctxs       = undefined -- SIS.paccum ctxs_0 ctxs_1
        logws      = map (exp . unLogP . particleLogProb) ctxs
    -- Select particles to continue with
    idxs <- replicateM (length ctxs_1) $ lift (sample (Categorical logws))
    handleResample ctxs (k idxs)