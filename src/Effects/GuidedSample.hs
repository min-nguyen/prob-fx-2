{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.GuidedSample where

import Comp
import Dist
import Trace
import Effects.Sample
import Env
import Effects.EnvRW

-- | The effect @GuidedSample@ for distributions with support for gradient log-pdfs
data GuidedSample a where
  GuidedSample :: (Distribution d, DiffDistribution q, a ~ Base d, a ~ Base q)
         => d              -- ^ model distribution
         -> q              -- ^ guide distribution
         -> Addr           -- ^ address of operation
         -> GuidedSample a        -- ^ sample

-- | For directly calling Param with a known runtime address,
guidedSample :: (Member GuidedSample es, Dist d a, DiffDist q a) => d -> q -> Addr -> Comp es a
guidedSample d q α = call (GuidedSample d q α)

-- | For directly calling Param for a variable in the model environment.
--   The value returned is written to the output environment.
guidedSample' :: forall env es x d q a.
  (Observable env x a, Members [EnvRW env, GuidedSample] es, Dist d a, DiffDist q a)
  => d -> q -> (Var x, Int) -> Comp es a
guidedSample' d q (varx, idx) = do
  let tag = varToStr varx
  y <- guidedSample d q (Addr tag idx)
  call (EnvWrite @env varx y)
  pure y