{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{- | Non-determinism effect.
-}

module Effects.NonDet (
    NonDet(..)
  , asum
  , asumWeaken
  , branch
  , branchWeaken
  , weakenNonDet
  , handleNonDet
  ) where

import Control.Applicative ( Alternative((<|>), empty) )
import Prog ( call, discharge, weaken, EffectSum, Member, Prog(..) )

-- |  The effect for non-deterministic computations
data NonDet a where
  -- | The operation @Empty@ represents there being no computation branches.
  Empty  :: NonDet a
  -- | The operation @Choose@ is parameterised by a boolean flag, indicating whether to take the left or right computation branch.
  Choose :: NonDet Bool

instance Member NonDet es => Alternative (Prog es) where
  empty  = call Empty
  m1 <|> m2 = do b <- call Choose
                 if b then m1 else m2

-- | Fold a collection of programs into a non-deterministic computation
asum :: (Foldable t, Member NonDet es) => t (Prog es a) -> Prog es a
asum = foldr (<|>) (call Empty)

-- | Fold a collection of programs into a non-deterministic computation
asumWeaken :: (Functor t, Foldable t, Member NonDet es) => t (Prog es a) -> Prog (NonDet : es) a
asumWeaken = asum . fmap weakenNonDet

-- | Branch a program into @n@ computations
branch :: Member NonDet es => Int -> Prog es a -> Prog es a
branch n prog = asum (replicate n prog)

-- | Install the non-determinism effect to a program and then branch into @n@ computations
branchWeaken :: Int -> Prog es a -> Prog (NonDet : es) a
branchWeaken n (Val x)   = asum $ replicate n (Val x)
branchWeaken n (Op op k) = asum $ replicate n (Op (weaken op) (weaken' . k))
  where weaken' (Op op k) = Op (weaken op) (weaken' . k)
        weaken' (Val x)   = Val x

-- | Install the non-determinism effect to a program
weakenNonDet :: Prog es a -> Prog (NonDet : es) a
weakenNonDet = branchWeaken 1

-- | Handle the @NonDet@ effect by running all computation branches
handleNonDet :: Prog (NonDet ': es) a -> Prog es [a]
handleNonDet (Val x) = pure [x]
handleNonDet (Op op k) = case discharge op of
   Right Choose -> (<|>) <$> handleNonDet (k True) <*> handleNonDet (k False)
   Right Empty  -> Val []
   Left op  -> Op op (handleNonDet . k)
