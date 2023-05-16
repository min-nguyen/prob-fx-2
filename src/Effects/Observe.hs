{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE FlexibleContexts #-}

module Effects.Observe where

import Comp
import Dist
import Trace
import Env
import Effects.EnvRW

-- | The effect @Observe@ for conditioning against observed values
data Observe a where
  Observe :: (Dist d a)
          => d              -- ^ distribution to condition with
          -> a              -- ^ observed value
          -> Addr           -- ^ address of @Observe@ operation
          -> Observe a

observe :: (Member Observe es, Dist d a)
       => d -> a -> Addr -> Comp es a
observe d y α = call (Observe d y α)

-- | For projecting and then successfully pattern matching against @Observe@
pattern ObsPrj :: (Member Observe es) => (Distribution d, x ~ Base d) => d -> x -> Addr -> EffectSum es x
pattern ObsPrj d y α <- (prj -> Just (Observe d y α))

-- | For discharging and then successfully pattern matching against @Observe@
pattern ObsDis :: () => (Distribution d, x ~ Base d) => d -> x -> Addr -> EffectSum (Observe : es) x
pattern ObsDis d y α <- (discharge -> Right (Observe d y α))