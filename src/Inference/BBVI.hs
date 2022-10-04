{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{- | Likelihood-Weighting inference.
-}

module Inference.BBVI
  where

import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad ( replicateM )
import Effects.Dist ( Sample, Observe(..), Dist )
import Effects.Lift ( handleLift, Lift )
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP(unLogP) )
import Inference.SIM as SIM (handleSamp)
import Model ( handleCore, Model )
import PrimDist ( logProb )
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler )


-- handleSamp :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
-- handleSamp (Val x) = return x
-- handleSamp (Op op k) = case discharge op of
--   Right (Sample d α) ->
--     do  x <- lift $ sample d
--         handleSamp (k x)
--   Left op' -> do
--      Op op' (handleSamp  . k)