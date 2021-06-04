{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
{-# LANGUAGE TypeOperators #-}
module Extensible.Inference.MH where

import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModel, runModelFree)
import Extensible.Sampler

-- runLW :: Freer '[Observe, Sample] a -> IO (a, Double)
-- runLW = runSample . runObserve

-- runObserve :: Freer (Observe : rs) a -> Freer rs (a, Double)
-- runObserve = loop 0
--   where
--   loop :: Double -> Freer (Observe : rs) a -> Freer rs (a, Double)
--   loop p (Pure x) = return (x, p)
--   loop p (Free u k) = case decomp u of 
--     Right (Observe d y α)  
--       -> let p' = logProb d y
--          in  loop (p + p') (k y) 
--     Left  u'  -> Free u' (loop p . k)

-- runSample :: Freer '[Sample] a -> IO a
-- runSample = sampleIO . loop
--   where
--   loop :: Freer '[Sample] a -> Sampler a
--   loop (Pure x) = return x
--   loop (Free u k) = case prj u of
--     Just (Sample d α) -> 
--        liftS (putStrLn $ ">> : " ++ show α) >> sample d >>= loop . k
--     Nothing         -> error "Impossible: Nothing cannot occur"
