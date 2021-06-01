{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
module Extensible.Inference where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Dist
import Model hiding (runModel, runModelFree)
import FreeT
import Sample
import Example
