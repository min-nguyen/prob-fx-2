{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Demo where

import Comp
import Env
import Trace hiding (null)
import Effects.Observe
import Effects.Sample
import Dist
import Model hiding (Model)
import qualified Model (Model)
import Sampler (handleImpure)

type Model a = Model.Model '[IO] a

class Member e es => e ∈ es
instance Member e es => e ∈ es

data Error e a where
  Error :: e -> Error e a

data Console a where
  GetLine     :: Console String
  PutStrLn    :: String -> Console ()

prog :: Console ∈ es => Comp es ()
prog = do
  s <- call GetLine
  call (PutStrLn s)

-- | Impure
handleConsole :: forall es a. IO ∈ es
  => Handler Console es a a
handleConsole = handle Val hop  where
  hop :: Console x -> (() -> x -> Comp es a) -> Comp es a
  hop (PutStrLn msg) k = do call (Prelude.putStrLn msg)
                            k () ()
  hop GetLine k        = do s <- call Prelude.getLine
                            k () s

runProg :: IO ()
runProg = (handleImpure . handleConsole) prog

-- | Pure
handleConsolePure :: forall es a. Error () ∈ es
  => String -> Handler Console es a (a, String)
handleConsolePure s = handleWith s hval hop  where
  hval :: String -> a -> Comp es (a, String)
  hval s x = Val (x, s)
  hop :: String ->  Console x -> (String -> x -> Comp es b) -> Comp es b
  hop s (PutStrLn msg) k = do k (msg ++ s) ()
  hop s GetLine k        = if null s
                              then call (Error ())
                              else k s s

handleError :: forall e es a. (e -> Comp es a) -> Handler (Error e) es a a
handleError catch = handle Val hop  where
  hop :: Error e x -> (() -> x -> Comp es a) -> Comp es a
  hop (Error e) k = catch e

runProgPure :: String -> ((), String)
runProgPure msg = (handlePure . handleError (\() -> return ((), "ERROR")) . handleConsolePure msg) prog


-- -- | Lin Regr
-- linRegr :: [Double] -> [Double] -> Model [Double]
-- linRegr xs ys = do
--   m        <- call $ Sample (mkNormal 0 3) (Addr "m" 0)
--   c        <- call $ Sample (mkNormal 0 2) (Addr "c" 0)
--   zipWithM (λ(x, y) → call $ Observe (mkNormal (m * x + c) 1) y ) xs ys