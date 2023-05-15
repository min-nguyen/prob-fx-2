{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Example where

import Comp
import Sampler (handleImpure)

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