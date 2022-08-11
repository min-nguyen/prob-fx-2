{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Resample where

import Control.Monad
import Prog
import Sampler
import PrimDist
import Effects.Lift
import LogP
-- import Inference.SIS

data Resample ctx a where
  Resample :: ([a], [ctx]) -> Resample ctx ([a], [ctx])
