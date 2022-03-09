{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Extensible.Experiment where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import Extensible.ModelEnv
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.NonDet
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.State
import Extensible.STrace
import Extensible.Sampler
import Extensible.Writer
import Debug.Trace

run' :: [Int]
run' = run $ handleProgram @_ @String (program1 <|> program2)

runTwice :: Member NonDet es => Prog es a -> Prog es a
runTwice m = do
  m <|> m

handleProgram :: Show a => Monoid w => Prog (Writer w : NonDet : es) a -> Prog es [a]
handleProgram (Val x) = return [x]
handleProgram prog = do
  progs_ws <- (runNonDet . runWriterX) prog
  -- When all of progs_ws are Vals, asum will produce Val <|> Val
  let progs = foldVals (map fst progs_ws)
  case progs
    of Right xs -> xs
       Left  ys -> handleProgram (asum ys)

mergeVals :: Show a => [Prog es' a] -> Prog es (Maybe [a])
mergeVals (Val x:vs) = do trace ("Val is " ++ show x) return ()
                          xs <- mergeVals vs
                          Val ((x:) <$> xs)
mergeVals [] = Val (Just [])
mergeVals vs = do trace ("Val is " ++ "Nothing") return ()
                  Val Nothing

program1 :: Member (Writer String) es => Prog es Int
program1 = do
  return 5

program2 :: Member (Writer String) es => Prog es Int
program2 = do
  tell "hi"
  tell "hi"
  return 6

runWriterX :: forall w ts a . Monoid w => Prog (Writer w ': ts) a -> Prog ts (Prog (Writer w ': ts) a, w)
runWriterX = loop mempty where
  loop ::  w -> Prog (Writer w ': ts) a -> Prog ts (Prog (Writer w ': ts) a, w)
  -- At this point, all Reader requests have been handled
  loop w (Val x) = return (Val x, w)
  -- Handle if Writer request, else ignore and go through the rest of the tree
  loop w (Op u k) = case decomp u of
    Right (Tell w') -> Val (k (), w')
    Left u'         -> Op u' (loop w . k)
