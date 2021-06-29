{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Extensible.Inference.LW where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Extensible hiding (Member)
import Extensible.Reader
import Control.Monad
import Control.Monad.Trans.Class
import Unsafe.Coerce
import Extensible.Dist
import qualified Extensible.Example as Example
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import Extensible.State
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum(..))

type Vals = '[Int, Double, Bool]

type Ⲭ = Map Addr (OpenSum Vals)

updateMapⲬ :: OpenSum.Member x Vals => Addr -> x -> Ⲭ -> Ⲭ
updateMapⲬ α x = Map.insert α (OpenSum.inj x) :: Ⲭ -> Ⲭ

-- | Perhaps this should also return a list of samples.
-- | Run LW n times for multiple data points
lw :: (es ~ '[Reader (MRec env), Dist, Observe, Sample])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env es a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [MRec env]                       -- List of model observed variables
   -> Sampler [(a, Double)]            -- List of n likelihood weightings for each data point
lw n model xs ys = do
  concat <$> zipWithM (\x y -> lwNsteps n y (model x)) xs ys

-- | Run LW n times for a single data point
lwNsteps :: (es ~ '[Reader (MRec env), Dist, Observe, Sample])
  => Int
  -> MRec env
  -> Model env es a
  -> Sampler [(a, Double)]
lwNsteps n env model = replicateM n (runLW env model)

-- | Run LW once for single data point
runLW :: MRec env -> Model env '[Reader (MRec env), Dist, Observe, Sample] a
      -> Sampler (a, Double)
runLW env = runSample . runObserve . runDist . runReader env . runModel

transformLW :: (Member (State Ⲭ) rs, Member Sample rs)
  => Freer rs a -> Freer rs a
transformLW = loop
  where
  loop :: (Member (State Ⲭ) rs, Member Sample rs)
       => Freer rs a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = do
    case u of
      Samp d α
        -> case d of
              -- We can unsafe coerce x here, because we've inferred the type of x from the distribution's type
              DistDouble d -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Double)) >>
                                            loop (k x))
              DistBool d   -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Bool)) >>
                                            loop (k x))
              DistInt d    -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Int)) >>
                                            loop (k x))
      _ -> Free u (loop . k)

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
