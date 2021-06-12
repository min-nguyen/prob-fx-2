{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Extensible.Inference.Basic where

import Data.Extensible hiding (Member)
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModel, runModelFree)
import Extensible.Sampler
import Extensible.Reader
import Extensible.Example as Example

run' :: forall s a. (forall rs. Model s rs a) -> MRec s -> IO ()
run' m env = do
  y <-  (runSample . runReader env . runObserve . runDist)
            (m @('[Dist, Observe, Reader (MRec s), Sample]))
  return ()

g = run' @LinRegrEnv (Example.linearRegression @_ @LinRegrEnv 0 0 1)

-- runBasic :: MRec env 
--          -> Model env '[Dist, Observe, Reader (MRec env), Sample] a
--          -> IO a 
runBasic :: env -> Freer '[Dist, Observe, Reader env, Sample] a -> IO a
runBasic env = runSample . runReader env . runObserve . runDist

runObserve :: Freer (Observe : rs) a -> Freer rs  a
runObserve = loop 
  where
  loop :: Freer (Observe : rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of 
    Right (Observe d y α) 
      -> let p = logProb d y
         in  loop (k y) 
    Left  u'  -> Free u' (loop . k)

runSample :: Freer '[Sample] a -> IO a
runSample = sampleIOFixed . loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = case prj u of
     Just (Sample d α) -> liftS (putStrLn $ ">> : " ++ show α) >> sample d >>= loop . k
     Nothing         -> error "Impossible: Nothing cannot occur"
 