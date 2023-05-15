{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWithM_" #-}

module Effects.Demo where

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

handleImpure' :: Comp '[Sampler] a -> IO a
handleImpure' = sampleIO . handleImpure

----------------------------------

{--####  A DETOUR  ####--}

-- | ## An effect computation
data Console a where
  GetLine   :: Console String
  PutStr    :: String -> Console ()

data Error e a where
  Error :: e -> Error e a

prog :: Console ∈ es => Comp es ()
prog = do
  s <- call GetLine
  call (PutStr s)

-- | ## Impure handling
handleConsole :: forall es a. IO ∈ es
  => Handler Console es a a
handleConsole = handle Val hop  where
  hop :: Console x -> (() -> x -> Comp es a) -> Comp es a
  hop (PutStr msg) k = do call (Prelude.putStr msg)
                          k () ()
  hop GetLine k      = do s <- call Prelude.getLine
                          k () s

runProg :: IO ()
runProg = (handleImpure . handleConsole) prog

-- | ## Pure handling
handleConsolePure :: forall es a. Error () ∈ es
  => String -> Handler Console es a (a, String)
handleConsolePure s = handleWith s hval hop  where
  hval :: String -> a -> Comp es (a, String)
  hval s x = Val (x, s)
  hop :: String ->  Console x -> (String -> x -> Comp es b) -> Comp es b
  hop s (PutStr msg) k = do k (msg ++ s) ()
  hop s GetLine k        = if null s
                              then call (Error ())
                              else k s s

handleError :: forall e es a. (e -> Comp es a) -> Handler (Error e) es a a
handleError catch = handle Val hop  where
  hop :: Error e x -> (() -> x -> Comp es a) -> Comp es a
  hop (Error e) k = catch e

runProgPure :: String -> ((), String)
runProgPure msg = (handlePure . handleError (\() -> return ((), "ERROR")) . handleConsolePure msg) prog


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
  mws <- (replicateM 1000 . handleImpure' . defaultSample . likelihood) (linRegr xs ys)
  return (mws :: [(Double, LogP)])

