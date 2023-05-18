{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWithM_" #-}
{-# HLINT ignore "Eta reduce" #-}

module EffDemo where

import Comp
import Env
import Trace hiding (map, null)
import Effects.Observe
import Effects.Sample
import Dist
import Model hiding (Model)
import qualified Model (Model)
import Sampler
import Inference.MC.LW as LW
import Inference.MC.SIM as SIM
import Control.Monad
import LogP

----------------------------------

class    Member e es => e ∈ es
instance Member e es => e ∈ es

type Model a = Model.Model '[Sampler] a

α :: Addr
α = Addr "" 0

runImpure' :: Comp '[Sampler] a -> IO a
runImpure' = sampleIO . runImpure

----------------------------------

{--####  A DETOUR  ####--}

-- | ## An effect computation
data Console a where
  GetLine   :: Console String
  PutStr    :: String -> Console ()

data Error a where
  Throw :: Int -> Error a

prog :: Console ∈ es => Comp es ()
prog = do
  s <- call GetLine
  _ <- call (PutStr (s ++ "!!"))
  return ()

-- | ## Impure handling
handleConsoleImpure :: forall es a. IO ∈ es
  => Handler Console es a a
handleConsoleImpure = handle hval hop  where
  hop :: Console x -> (x -> Comp es a) -> Comp es a
  hop GetLine k      = do s <- call Prelude.getLine
                          k s
  hop (PutStr msg) k = do call (Prelude.putStr msg)
                          k ()
  hval x             = return x

runProgImpure :: IO ()
runProgImpure = (runImpure . handleConsoleImpure) prog

-- | ## Pure handling
handleConsolePure :: forall es a. Error ∈ es
  => Handler Console es a (a, String)
handleConsolePure = handleWith "" hval hop  where
  hop :: String ->  Console x -> (String -> x -> Comp es b) -> Comp es b
  hop s GetLine k      = k s s
  hop s (PutStr msg) k = if length msg < 2 then k (msg ++ s) ()
                                           else call (Throw 0)
  hval s x             = return (x, s)

handleError :: forall es a. (Int -> Comp es a) -> Handler Error es a a
handleError catch = handle Val hop  where
  hop :: Error x -> (x -> Comp es a) -> Comp es a
  hop (Throw e) k = catch e

runProgPure :: ((), String)
runProgPure = (runPure . handleError catch . handleConsolePure) prog
  where catch errcode = return ((), show errcode)

----------------------------------

{--####  PROBABILISTIC PROGRAMMING WITH ALGEBRAIC EFFECTS  ####--}

-- | ## Linear regression
linRegr :: [Double] -> [Double] -> Model Double
linRegr xs ys = do
  m        <- call (Sample (Normal 0 3) α)
  c        <- call (Sample (Normal 0 2) α)
  zipWithM (\x y -> do call (Observe (Normal (m * x + c) 1) y α)) xs ys
  return m

-- | ## Likelihood weighting over linear regression
lwLinRegr :: IO [(Double, LogP)]
lwLinRegr = do
  let xs      = [0 .. 10]
      ys      = map (*3) xs
  -- Get the sampled m's and their likelihood-weighting
  mws <- (replicateM 1000 . runImpure' . defaultSample . likelihood) (linRegr xs ys)
  return (mws :: [(Double, LogP)])

{-
  ./prob-fx.sh lwLinRegrOnce
-}