{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
{-# LANGUAGE TypeOperators #-}
module Extensible.Inference where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModel, runModelFree)
import Sample

-- runModelFree :: Model s rs a 
--          -> Freer rs a
-- runModelFree model = do
--   let loop v = do
--         x <- runFreeT v
--         case x of 
--           Free dist -> do
--             a  <- lift $ sample dist 
--             loop a 
--           Pure v -> return v
--   loop model 
