{-# LANGUAGE PolyKinds #-}

module Extensible.Inference.Inf where

import Extensible.Model

runInf :: (b -> Model s es a)              -- Model
       -> [b]                              -- Model inputs
       -> [MRec s]                         -- Model observed values
       -> (MRec s -> Model s es a -> IO c) -- Inference method
       -> IO [c]
runInf model xs ys inf = do
  let models = map model xs
      infs   = map inf ys
      ys'    = zipWith ($) infs models
  sequence ys'
