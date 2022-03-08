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

runNonDet :: Prog (NonDet ': es) a -> Prog es [a]
runNonDet (Val x) = return [x]
runNonDet (Op op k) = case op of
   Choose' -> (<|>) <$> runNonDet (k True) <*> runNonDet (k False)
   Empty'  -> Val []
   Other op  -> Op op (runNonDet . k)


(<||>) :: Prog es a -> Prog es a -> Prog es [a]
(Val x) <||> (Val y) = Val [x,y]

-- handleRelay' :: Prog (NonDet ': es) a -> Prog es [a]
-- handleRelay'   (Val x) = pure [x]
-- handleRelay'   (Op u k) =
--   case decomp u of
--     Right op -> h op (handleRelay'  . k)
--     Left  u' -> Op u' (handleRelay'  . k)
--   where h op k = case op of
--           NNil -> pure []
--           NCons p q -> concat <$> sequence [k p, k q]