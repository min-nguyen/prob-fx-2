{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Sample where

import Prog
import PrimDist
import Trace
import Effects.ObsRW
import Env

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist d a
          => d              -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => PrimDist d a => d -> Addr -> EffectSum es a
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show a) => PrimDist d a => d -> Addr -> EffectSum (Sample : es) a
pattern SampDis d α <- (discharge -> Right (Sample d α))


-- sample :: forall env es x d a. (Member (ObsRW env) es, Observable env x a, PrimDist d a)
--        => d -> Var x -> Prog es ()
-- sample d x = do
--   maybe_y <- oAsk @env x
--   -- call (Sample d (0, ))
--   pure ()