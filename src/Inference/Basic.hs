{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Inference.Basic where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model
import FreeT
import Sample

-- runModel :: ModelT s Sampler a -> Sampler (Reader (MRec s) a)
runModel :: FreeT Dist (ReaderT (Record (Maybes s)) Sampler) a 
         -> (ReaderT (Record (Maybes s)) Sampler) a
runModel model = do
  let loop v = do
        x <- runFreeT v
        case x of 
          FreeF dist -> do
            a  <- lift $ sample dist 
            loop a 
          Pure v -> return v
  loop model


