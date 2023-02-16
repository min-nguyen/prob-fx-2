{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.Score where

import Comp
import PrimDist
import Trace
import Effects.Sample
import Env
import Effects.EnvRW
import Effects.Dist (Dist)

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Guide a where
  Guide :: (Distribution d, DiffDistribution q, a ~ Base d, a ~ Base q)
         => d              -- ^ model distribution
         -> q              -- ^ guide distribution
         -> Addr           -- ^ address of operation
         -> Guide a        -- ^ sample

-- | For projecting and then successfully pattern matching against @Param@
pattern GuidePrj :: (Member Guide es) => (PrimDist d a, DiffDist q a) => d -> q -> Addr -> EffectSum es a
pattern GuidePrj d q α <- (prj -> Just (Guide d q α))

-- | For directly calling Param with a known runtime address,
guide :: (Member Guide es, PrimDist d a, DiffDist q a) => d -> q -> Addr -> Comp es a
guide d q α = call (Guide d q α)

-- | For directly calling Param for a variable in the model environment.
--   The value returned is written to the output environment.
guide' :: forall env es x d q a.
  (Observable env x a, Members [EnvRW env, Guide] es, PrimDist d a, DiffDist q a)
  => d -> q -> (Var x, Int) -> Comp es a
guide' d q (varx, idx) = do
  let tag = varToStr varx
  y <- guide d q (Addr tag idx)
  call (Write @env varx y)
  pure y