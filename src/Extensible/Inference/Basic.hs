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
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import Extensible.Reader
import Extensible.Example as Example

runBasic :: MRec env -> Model env '[Reader (MRec env), Dist, Observe, Sample] a -> 
              IO a 
runBasic env m = runSample $ runObserve $ runDist $ runReader env $ runModel m

g :: IO Double
g = runBasic (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression 0 0 0)

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
 