{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Extensible.Member where

newtype P t rs = P {unP :: Int}

class FindElem x ts where
  findElem :: P x ts
