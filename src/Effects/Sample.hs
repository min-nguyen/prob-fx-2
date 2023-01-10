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
import Effects.ObsRW
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
       => d -> Prog es a
sample d = call (Sample d α)
  where α = Addr 0 "" 0

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => PrimDist d a => d -> Addr -> EffectSum es a
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show a) => PrimDist d a => d -> Addr -> EffectSum (Sample : es) a
pattern SampDis d α <- (discharge -> Right (Sample d α))

sample' :: forall env es x d a.
  (Observable env x a, Members [ObsRW env, Sample] es,  PrimDist d a)
  => d -> Var x -> Prog es a
sample' d varx = do
  maybe_y <- call (OAsk @env varx)
  y       <- case maybe_y of
                Nothing -> sample d
                Just y  -> sample (Deterministic d y)
  call (OTell @env varx y)
  pure y