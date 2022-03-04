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
  Tell :: w -> Writer w ()

tell :: Member (Writer w) ts => w -> Prog ts ()
tell w = Op (inj $ Tell w) Val

runWriter :: forall w ts a . Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
runWriter = loop mempty where
  loop ::  w -> Prog (Writer w ': ts) a -> Prog ts (a, w)
  -- At this point, all Reader requests have been handled
  loop w (Val x) = return (x, w)
  -- Handle if Writer request, else ignore and go through the rest of the tree
  loop w (Op u k) = case decomp u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

runWriter' :: Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
runWriter'  = handleRelay (\x -> return (x, mempty))
  (\(Tell w') k -> k () >>= \(x, w) -> return (x, w' <> w))

runWriter'' :: Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
runWriter'' = handleRelaySt mempty (\w a -> return (a, w))
  (\w (Tell w') k -> k (w <> w') ())