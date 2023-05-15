{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Comp
import Sampler (handleIO)

class Member e es => e ∈ es
instance Member e es => e ∈ es

data Exception e a where
  Error :: e -> Exception e a

data Console a where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String

prog :: Console ∈ es => Comp es ()
prog = do
  s <- call GetLine
  call (PutStrLn s)

hdlConsoleImpure :: Comp '[Console, IO] a -> IO a
hdlConsoleImpure = handleIO . handle Val hop  where
  hop :: Console a -> (() -> a -> Comp '[IO] b) -> Comp '[IO] b
  hop (PutStrLn msg) k = do call (Prelude.putStrLn msg)
                            k () ()
  hop GetLine k        = do s <- call Prelude.getLine
                            k () s

runProgImpure :: IO ()
runProgImpure = hdlConsoleImpure prog

hdlConsolePure :: forall es a. (Exception () ∈ es) => String -> Comp (Console:es) a -> Comp es (a, String)
hdlConsolePure s = handleWith s hval hop  where
  hval :: String -> a -> Comp es (a, String)
  hval s x = Val (x, s)
  hop :: String ->  Console x -> (String -> x -> Comp es b) -> Comp es b
  hop s (PutStrLn msg) k = do k (msg ++ s) ()
  hop s GetLine k        = if null s
                              then call (Error ())
                              else undefined

hdlException :: forall e es a. (e -> Comp es a) -> Comp (Exception e:es) a -> Comp es a
hdlException catch = handle Val hop  where
  hop :: Exception e x -> (() -> x -> Comp es a) -> Comp es a
  hop (Error e) k = catch e

runProgPure :: String -> ((), String)
runProgPure msg = (run . hdlException (\() -> return ((), "")) . hdlConsolePure msg) prog