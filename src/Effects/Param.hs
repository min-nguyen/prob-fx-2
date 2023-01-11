{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.Param where

import Prog
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
         -> Param a        -- ^ observed point

param :: forall d a es. (Member Param es, DiffDist d a)
       => d -> Addr -> Prog es a
param d α = call (Param d α)

-- | For projecting and then successfully pattern matching against @Param@
pattern ParamPrj :: (Member Param es) => (DiffDistribution d, x ~ Base d) => d -> Addr -> EffectSum es x
pattern ParamPrj q α <- (prj -> Just (Param q α))

-- | The effect @Score@ is like @Param@ but retains the original distribution to be optimised
data Score a where
  Score  :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ original distribution
         -> d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Score a        -- ^ observed point

-- | For projecting and then successfully pattern matching against @Score@
pattern ScorePrj :: (Member Score es) => (DiffDistribution d, x ~ Base d) => d -> d -> Addr -> EffectSum es x
pattern ScorePrj d q α <- (prj -> Just (Score d q α))

param' :: forall env es x d a.
  (Observable env x a, Members [EnvRW env, Param] es, DiffDist d a)
  => d -> Var x -> Prog es a
param' d varx = do
  let tag = varToStr varx
  y <- param d (Addr tag 0)
  call (Write @env varx y)
  pure y