{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Extensible.Inference.Basic where

import Data.Extensible hiding (Member)
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import Extensible.Reader
import Extensible.Example as Example

basic :: (es ~ '[Dist, Observe, Reader (LRec env), Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> Sampler [a]
basic n model xs ys = do
  concat <$> zipWithM (\x y -> basicNsteps n y (model x)) xs ys

basicNsteps :: (es ~ '[Dist, Observe, Reader (LRec env), Sample])
  => Int
  -> LRec env
  -> Model env es a
  -> Sampler [a]
basicNsteps n ys model = replicateM n (runBasic ys model)

runBasic :: LRec env -> Model env '[Dist, Observe, Reader (LRec env), Sample] a -> Sampler a
runBasic ys m = runSample $ runReader ys $ runObserve $ runDist $ runModel m

runObserve :: Freer (Observe : rs) a -> Freer rs  a
runObserve = loop
  where
  loop :: Freer (Observe : rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right (Observe d y α)
      -> let p = logProb d y
         in  loop (k y)
    Left  u'  -> Free u' (loop . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = case prj u of
     Just (Sample d α) -> sample d >>= loop . k
     Nothing           -> error "Impossible: Nothing cannot occur"
