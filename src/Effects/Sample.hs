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

sample :: (Member Sample es, PrimDist d a)
       => d -> Prog es a
sample d = do
  call (Sample d (Addr 0 "" 0))

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => PrimDist d a => d -> Addr -> EffectSum es a
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show a) => PrimDist d a => d -> Addr -> EffectSum (Sample : es) a
pattern SampDis d α <- (discharge -> Right (Sample d α))

-- | The effect @Sample@ for sampling from distirbutions
data Sample' (env :: [Assign Symbol *]) a where
  Sample' :: forall env x d a. (PrimDist d a, Observable env x a)
          => d              -- ^ distribution to sample from
          -> Var x
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample' env a

sample' :: forall env es x d a. (Member (Sample' env) es, Observable env x a, PrimDist d a)
       => d -> Var x -> Prog es a
sample' d x = do
  call (Sample' @env d x (Addr 0 "" 0))


defaultSample'
  :: Env env
  -> Prog '[Sample' env] a
  -> Sampler a
defaultSample' env (Val x)   = return x
defaultSample' env (Op op k) = case discharge1 op of
  (Sample' d x α) -> do let vs       = get x env
                            maybe_v  = safeHead vs
                        v <- maybe (fmap (draw d) sampleRandom) pure maybe_v
                        (defaultSample' env . k) v