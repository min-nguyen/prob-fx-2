{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Inference.Basic where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model hiding (runModel, runModelFree)
import FreeT
import Sample
import Example

-- runModel :: ModelT s Sampler a -> Sampler (Reader (MRec s) a)
runModelFree :: Show a => ModelT s Sampler a 
         -> ReaderT (MRec s) Sampler a
runModelFree model = do
  let loop v = do
        x <- runFreeT v
        case x of 
          FreeF dist -> do
            a  <- lift $ sample dist 
            loop a 
          Pure v -> return v
  loop model 

runModel :: Show a => ModelT s Sampler a -> MRec s -> IO a
runModel model = sampleIO . runReaderT (runModelFree model)

exampleRun = runModel (linearRegression 0 0 0) (y @= Nothing <: nil)
-- runFull :: FreeT Dist (ReaderT (MRec s) Sampler) a 
--         -> (ReaderT (MRec s) Sampler) a
-- runFull :: forall s a. (HasVar s "y" Double) => MRec s -> FreeT Dist (ReaderT (MRec s) Sampler) a -> IO a
-- runFull env = sampleIO . flip runReaderT env . runModel
 
-- exampleEnv :: MRec LinRegrEnv
-- exampleEnv =  (y @= Nothing <: nil) 

-- runEx :: IO Double
-- runEx = runFull exampleEnv (linearRegression 0 0 0)