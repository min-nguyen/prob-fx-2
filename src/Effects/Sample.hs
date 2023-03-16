{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.Sample where

import Comp
import Dist
import Trace
import Effects.EnvRW
import Env
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Sampler
import Util
import Data.Maybe

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: Dist d a
          => d              -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => Dist d a => d -> Addr -> EffectSum es a
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show a) => Dist d a => d -> Addr -> EffectSum (Sample : es) a
pattern SampDis d α <- (discharge -> Right (Sample d α))

-- | For directly calling Sample with a known runtime address
sample :: forall d a es. (Member Sample es, Dist d a)
       => d -> Addr -> Comp es a
sample d α = call (Sample d α)

-- | For directly calling Sample for a variable in the model environment.
--   The attempts to first use an existing sampled value in the input environment.
--   The sampled value is written to an output environment.
sample' :: forall env es x d a. (Observable env x a, Members [EnvRW env, Sample] es,  Dist d a)
  => d -> (Var x, Int) -> Comp es a
sample' d (varx, idx) = do
  let tag = varToStr varx
  maybe_y <- call (EnvRead @env varx)
  y       <- case maybe_y of
                Nothing -> sample d (Addr tag idx)
                Just y  -> sample (Deterministic d y) (Addr tag idx)
  call (EnvWrite @env varx y)
  pure y