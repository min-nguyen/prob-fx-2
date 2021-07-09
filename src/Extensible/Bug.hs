{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Extensible.Bug where

import Extensible.Freer
import Extensible.Dist
import Extensible.Sampler
import Data.Extensible hiding (Member)
import Control.Lens hiding ((:>))
import Util

type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((f :> a) : as)   = ((f :> [a]) : AsList as)
  AsList '[] = '[]

type LRec s = Record (AsList s)

-- A Reader for which it is only possible to access record fields with type [a].
data RecReader env a where
  Ask :: Lens' (Record env) [a] -> RecReader env (Maybe a)

ask :: (Member (RecReader env) rs) =>
  Lens' (Record env) [a] -> Freer rs (Maybe a)
ask field = Free (inj $ Ask field) Pure

runReader :: Record env -> Freer (RecReader env ': rs) a -> Freer rs a
runReader env = loop where
  -- loop :: Freer (RecReader env ': rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right (Ask f) -> let ys = env ^. f
                         y  = maybeHead ys
                     in  loop (k y)
    Left  u'      -> Free u' (loop . k)

newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (RecReader (AsList env)) es) => Freer es a }
  deriving Functor

-- why doesn't this version work?
runBasic1 ::
 LRec env
  -> Model env '[Dist, Observe, RecReader (AsList env), Sample] a -> Sampler a
runBasic1 ys m
  = runSample $ runReader ys $ runObserve $ runDist $ runModel m
-- but this does:
runBasic2 :: (e ~ AsList env) =>
 LRec env
  -> Model env '[Dist, Observe, RecReader (AsList env), Sample] a -> Sampler a
runBasic2 ys m
  = runSample $ runReader ys $ runObserve $ runDist $ runModel m
-- or this:
runBasic3 ::
 LRec env
  -> Freer '[Dist, Observe, RecReader (AsList env), Sample] a -> Sampler a
runBasic3 ys m
  = runSample $ runReader ys $ runObserve $ runDist  m

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

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = case prj u of
     Just (Sample d α) -> sample d >>= loop . k
     Nothing           -> error "Impossible: Nothing cannot occur"
