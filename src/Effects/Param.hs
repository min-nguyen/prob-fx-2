{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.Param where

import Comp
import PrimDist
import Trace
import Effects.Sample
import Env
import Effects.EnvRW

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Param a where
  Param :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Param a        -- ^ sample

-- | For projecting and then successfully pattern matching against @Param@
pattern ParamPrj :: (Member Param es) => (DiffDistribution d, x ~ Base d) => d -> Addr -> EffectSum es x
pattern ParamPrj q α <- (prj -> Just (Param q α))

-- | For directly calling Param with a known runtime address,
param :: forall d a es. (Member Param es, DiffDist d a)
       => d -> Addr -> Comp es a
param d α = call (Param d α)

-- | For directly calling Param for a variable in the model environment.
--   The value returned is written to the output environment.
param' :: forall env es x d a.
  (Observable env x a, Members [EnvRW env, Param] es, DiffDist d a)
  => d -> (Var x, Int) -> Comp es a
param' d (varx, idx) = do
  let tag = varToStr varx
  y <- param d (Addr tag idx)
  call (Write @env varx y)
  pure y