{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE ViewPatterns #-}


{-# LANGUAGE FlexibleContexts #-}

module Effects.Param where

import Prog
import PrimDist
import Trace
import Effects.Sample

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Param a where
  Param :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Param a        -- ^ observed point

setParamDist :: (DiffDistribution d, a ~ Base d) => Param a -> d -> Param a
setParamDist (Param q α) q' = Param q' α

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

-- handleParams :: forall es a. Member Sample es => Prog (Param : es) a -> Prog es (a, GradTrace)
-- handleParams = loop Trace.empty where
--   loop :: GradTrace -> Prog (Param : es) a -> Prog es (a, GradTrace)
--   loop grads (Val a)   = pure (a, grads)
--   loop grads (Op op k) = case discharge op of
--     Right (Param (q :: d) α) -> do
--       x <- call (Sample q α)
--       let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
--       (loop grads' . k) x
--     Left op' -> Op op' (loop grads . k)
