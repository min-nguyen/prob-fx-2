{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Effects.Sample where

import Prog
import PrimDist
import Trace
import Effects.EnvRW
import Env
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Sampler
import Util
import Data.Maybe

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist d a
          => d              -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

sample :: forall d a es. (Member Sample es, PrimDist d a)
       => d -> Addr -> Prog es a
sample d α = call (Sample d α)

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => PrimDist d a => d -> Addr -> EffectSum es a
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show a) => PrimDist d a => d -> Addr -> EffectSum (Sample : es) a
pattern SampDis d α <- (discharge -> Right (Sample d α))

sample' :: forall env es x d a.
  (Observable env x a, Members [EnvRW env, Sample] es,  PrimDist d a)
  => d -> Var x -> Prog es a
sample' d varx = do
  let tag = varToStr varx
  maybe_y <- call (Read @env varx)
  y       <- case maybe_y of
                Nothing -> sample d (Addr tag 0)
                Just y  -> sample (Deterministic d y) (Addr tag 0)
  call (Write @env varx y)
  pure y