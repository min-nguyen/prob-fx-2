{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Extensible.Inference.LW where

import Data.Map
import Data.Extensible
import Extensible.Reader
import Control.Monad.Trans.Class
import Extensible.Dist
import qualified Extensible.Example as Example
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler

type Conts = forall a. Map Int (Freer '[Sample] a)

runLW :: MRec env -> Model env '[Reader (MRec env), Dist, Observe, Sample] a
      -> Sampler (a, Double)
runLW env = runSample . runObserve . runDist . runReader env . runModel

runObserve :: Freer (Observe : rs) a -> Freer rs (a, Double)
runObserve = loop 0
  where
  loop :: Double -> Freer (Observe : rs) a -> Freer rs (a, Double)
  loop p (Pure x) = return (x, p)
  loop p (Free u k) = case decomp u of
    Right (Observe d y α)
      -> let p' = prob d y
         in  loop (p + p') (k y)
    Left  u'  -> Free u' (loop p . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = case prj u of
    Just (Sample d α) ->
       liftS (putStrLn $ ">> : " ++ show α) >> sample d >>= loop . k
    Nothing         -> error "Impossible: Nothing cannot occur"
