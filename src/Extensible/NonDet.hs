{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
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

data NonDet a where
  NNil  :: NonDet a
  NCons :: a -> a -> NonDet a

infixr 5 :|:

pattern (:|:) :: x -> x -> EffectSum (NonDet : es) x
pattern p :|: q <- (decomp -> Right (NCons p q))

pattern NNil' <- (decomp -> Right NNil)

-- instance Member NonDet es => Alternative (Prog es) where
--   empty  = send Fail
--   (<|>)  = (:|:)

runNonDet :: Prog (NonDet ': es) a -> Prog es [a]
runNonDet (Val x) = return [x]
runNonDet (Op op k) = case  op of
   (p :|: q) -> do ps <- runNonDet (k p)
                   qs <- runNonDet (k q)
                   return (ps ++ qs)
   NNil'     -> return []
   Other op  -> Op op (runNonDet . k)
  --  ()
                -- in  map runNonDet ps
  -- runNonDet p



-- handleRelay' :: Prog (NonDet ': es) a -> Prog es [a]
-- handleRelay'   (Val x) = pure [x]
-- handleRelay'   (Op u k) =
--   case decomp u of
--     Right op -> h op (handleRelay'  . k)
--     Left  u' -> Op u' (handleRelay'  . k)
--   where h op k = case op of
--           NNil -> pure []
--           NCons p q -> concat <$> sequence [k p, k q]