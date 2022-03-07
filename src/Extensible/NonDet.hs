{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Extensible.NonDet where

import Control.Applicative
import Control.Monad
import Extensible.Freer

data NonDet a where
  MZero :: NonDet a
  MPlus :: NonDet Bool

instance Member NonDet es => Alternative (Prog es) where
  empty     = send MZero
  m1 <|> m2 = do b <- send MPlus
                 if b then m1 else m2