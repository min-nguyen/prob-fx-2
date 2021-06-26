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

module Extensible.Inference.Inf where

import Extensible.Model
import Extensible.Sampler

runInf :: (b -> Model s es a)              -- Model
       -> [b]                              -- Model inputs
       -> [MRec s]                         -- Model observed values
       -> (MRec s -> Model s es a -> Sampler c) -- Inference method
       -> Sampler [c]
runInf model xs ys inf = do
  let models = map model xs
      infs   = map inf ys
      ys'    = zipWith ($) infs models
  sequence ys'
