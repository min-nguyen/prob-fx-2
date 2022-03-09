{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Extensible.NonDet where

import Control.Applicative
import Control.Monad
import Extensible.Freer
import Extensible.Writer
import Debug.Trace

data NonDet a where
  Empty  :: NonDet a
  Choose :: NonDet Bool

pattern Choose' :: () => (x ~ Bool) => EffectSum (NonDet : es) x
pattern Choose' <- (decomp -> Right Choose)

pattern Empty' :: EffectSum (NonDet : es) x
pattern Empty' <- (decomp -> Right Empty)

instance Member NonDet es => Alternative (Prog es) where
  empty  = send Empty
  m1 <|> m2 = do b <- send Choose
                 if b then m1 else m2

-- runNonDet' :: Functor ctx => ctx (Prog (NonDet ': es) a) -> Prog es [ctx a]
-- -- runNonDet' (Val x) = return [x]
-- runNonDet' ctxop = fmap (runNonDet) ctxop

asum :: Member NonDet es => [Prog es a] -> Prog es a
asum progs = foldr (<|>) (send Empty) progs

runNonDet :: Prog (NonDet ': es) a -> Prog es [a]
runNonDet (Val x) = return [x]
runNonDet (Op op k) = case op of
   Choose' -> (<|>) <$> runNonDet (k True) <*> runNonDet (k False)
   Empty'  -> Val []
   Other op  -> Op op (runNonDet . k)

branch :: Member NonDet es => Int -> Prog es a -> Prog es a
branch n prog = asum (replicate n prog)

-- Given a list of programs, check whether they all terminate.
-- If a program is unfinished, return all programs
-- If all finished, return a single program that returns all results
foldVals :: Member NonDet es' => Show a => [Prog es' a] -> Either [Prog es' a] (Prog es [a])
foldVals ps  = loop ps where
  loop (Val x:vs) = do -- trace ("Val is " ++ show x) return ()
                       xs <- loop vs
                       Right ((x:) <$> xs)
  loop [] = Right (Val [])
  loop vs = Left ps