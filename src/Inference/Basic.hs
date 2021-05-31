module Inference.Basic where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model
import FreeT
import Sample

runModelFree :: Monad m => ModelT s m a -> ReaderT (MRec s) m a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model
