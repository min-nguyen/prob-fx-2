{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Inference.Basic where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model hiding (runModel)
import FreeT
import Sample
import Example

-- runModel :: ModelT s Sampler a -> Sampler (Reader (MRec s) a)
runModel :: FreeT Dist (ReaderT (MRec s) Sampler) a 
         -> ReaderT (MRec s) Sampler a
runModel model = do
  let loop v = do
        x <- runFreeT v
        case x of 
          FreeF dist -> do
            a  <- lift $ sample dist 
            loop a 
          Pure v -> return v
  loop model


-- runFull :: FreeT Dist (ReaderT (MRec s) Sampler) a 
--         -> (ReaderT (MRec s) Sampler) a
-- runFull :: forall s a. (HasVar s "y" Double) => MRec s -> FreeT Dist (ReaderT (MRec s) Sampler) a -> IO a
-- runFull env = sampleIO . flip runReaderT env . runModel
 
-- exampleEnv :: MRec LinRegrEnv
-- exampleEnv =  (y @= Nothing <: nil) 

-- runEx :: IO Double
-- runEx = runFull exampleEnv (linearRegression 0 0 0)