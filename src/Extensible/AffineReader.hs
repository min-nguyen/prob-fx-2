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

module Extensible.AffineReader where

import Extensible.State
import Extensible.Freer
-- import Data.Extensible hiding (Member)
import qualified Extensible.OpenProduct as OP
-- import Extensible.Model
import Control.Lens hiding ((:>))
import Util

type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((f, a) : as)   = ((f , [a]) : AsList as)
  AsList '[] = '[]

type LRec s = OP.OpenProduct (AsList s)

-- A Reader for which it is only possible to access record fields with type [a].
data AffReader env a where
  Ask :: Getting [a] (OP.OpenProduct env) [a]
      -> ASetter (OP.OpenProduct env) (OP.OpenProduct env) [a] [a]
      -> AffReader env (Maybe a)

ask :: Member (AffReader env) rs => Getting [a] (OP.OpenProduct env) [a]
  -> ASetter (OP.OpenProduct env) (OP.OpenProduct env) [a] [a]
  -> Freer rs (Maybe a)
ask g s = Free (inj $ Ask g s) Pure

runReader :: OP.OpenProduct env -> Freer (AffReader env ': rs) a -> Freer rs a
runReader env = loop where
  -- loop :: Freer (RecReader env ': rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right (Ask g s) -> let ys = env ^. g
                           y  = maybeHead ys
                       in  loop (k y)
    Left  u'      -> Free u' (loop . k)

-- | The only purpose of the State (LRec env) effect is to check if all observed values in the environment have been consumed.
runAffReader :: forall env rs a.
  OP.OpenProduct (AsList env) -> Freer (AffReader (AsList env) ': rs) a -> Freer rs (a, OP.OpenProduct (AsList env) )
runAffReader env = loop env where
  loop :: OP.OpenProduct (AsList env) -> Freer (AffReader (AsList env)  ': rs) a -> Freer rs (a, OP.OpenProduct (AsList env) )
  loop env (Pure x) = return (x, env)
  loop env (Free u k) = case decomp u of
    Right (Ask g s) -> do
      let ys = env ^. g
          y  = maybeHead ys
          env' = env & s .~ safeTail ys
      loop env' (k y)
    Left  u'  -> Free u' (loop env . k)
