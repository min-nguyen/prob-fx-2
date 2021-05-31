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

module Extensible.Writer where
import Extensible.Freer

data Writer w a where 
  Put :: w -> Writer w ()

put :: Member (Writer w) rs => w -> Freer rs ()
put w = Free (inj $ Put w) Pure

runWriter :: forall w rs a . Monoid w => Freer (Writer w ': rs) a -> Freer rs (a, w)
runWriter m = loop mempty m where
  loop ::  w -> Freer (Writer w ': rs) a -> Freer rs (a, w)
  -- At this point, all Reader requests have been handled
  loop w (Pure x) = return (x, w)
  -- Handle if Writer request, else ignore and go through the rest of the tree
  loop w (Free u k) = case decomp u of
    Right (Put w') -> loop (w `mappend` w') (k ())
    Left u'        -> Free u' (loop w . k)
