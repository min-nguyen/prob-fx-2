{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
module Inference where

import Data.Extensible
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model hiding (runModel, runModelFree)
import FreeT
import Sampler
import Example
import Control.Monad.Trans.Identity

-- runModel :: ModelT s Sampler a -> Sampler (Reader (MRec s) a)
runModelFree :: (MonadTrans t, Monad (t Sampler)) => ModelT s t a 
         -> ReaderT (MRec s) (t Sampler) a
runModelFree model = do
  let loop v = do
        x <- runFreeT v
        let handle d obs k =  
              if   isJust obs 
              then let p = logProb d in loop $ k (fromJust obs)
              else do lift (lift (sample d)) >>= loop
        case x of 
          FreeF d@(NormalDist _ _ obs k) -> handle d obs k
          FreeF d@(UniformDist _ _ obs k) -> handle d obs k
          FreeF d@(DiscrUniformDist _ _ obs k) -> handle d obs k
          FreeF d@(GammaDist _ _ obs k) -> handle d obs k
          FreeF d@(BetaDist _ _ obs k) -> handle d obs k
          FreeF d@(BinomialDist _ _ obs k) -> handle d obs k
          FreeF d@(BernoulliDist _  obs k) -> handle d obs k
          FreeF d@(CategoricalDist  _ obs k) -> handle d obs k
          FreeF d@(DiscreteDist  _ obs k) -> handle d obs k
          Pure v -> return v
  loop model 

runModel :: Show a => ModelT s IdentityT a -> MRec s -> IO a
runModel model = sampleIO . runIdentityT . runReaderT (runModelFree model)

exampleRun :: IO Double
exampleRun = runModel (linearRegression 0 0 0) (allNothing @LinRegrEnv)