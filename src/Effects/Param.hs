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

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Param a where
  Param :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Param a        -- ^ observed point

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

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Param' env a where
  Param' :: forall env x d a. (DiffDist d a, Observable env x a)
         => d              -- ^ proposal distribution
         -> (Var x, Addr)  -- ^ address of operation
         -> Param' env a   -- ^ observed point

param' :: forall env es x d a. (Member (Param' env) es, Observable env x a, DiffDist d a)
       => d -> Var x -> Prog es a
param' d x = call (Param' @env d (x, α))
  where α = Addr 0 "" 0

handleParams' :: forall env es a. Member (Sample' env) es =>  Prog (Param' env : es) a -> Prog es (a, GradTrace)
handleParams'  = loop Trace.empty where
  loop :: GradTrace -> Prog (Param' env : es) a -> Prog es (a, GradTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (Param' (q :: d) (x, α)) -> do
      x <- call (Sample' @env q x α)
      let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
      (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)
