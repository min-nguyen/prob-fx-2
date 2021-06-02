{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
module Inference where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model hiding (runModel, runModelFree)
import FreeT
import Sampler
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

exampleRun :: IO Double
exampleRun = runModel (linearRegression 0 0 0) (allNothing @LinRegrEnv)