{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeOperators, UndecidableInstances, MultiParamTypeClasses, TypeFamilyDependencies #-}

module FindElem where

import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )

newtype P t rs = P {unP :: Int}

class FindElem x ts where
  findElem :: P x ts

instance FindElem t (t ': r) where
  findElem = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' : r) where
  findElem = P $ 1 + unP (findElem :: P t r)

instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  findElem = error "unreachable"

type family Subset xs ys where
  Subset (x : xs) ys = (FindElem x ys, Subset xs ys)
  Subset '[] ys = ()
