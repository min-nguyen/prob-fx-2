{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Sample where

import Prog
import PrimDist
import Trace

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: (Distribution d, a ~ Base d)
          => d              -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => (Distribution d, x ~ Base d) => d -> Addr -> EffectSum es x
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show x) =>  (Distribution d, x ~ Base d) => d -> Addr -> EffectSum (Sample : es) x
pattern SampDis d α <- (discharge -> Right (Sample d α))
