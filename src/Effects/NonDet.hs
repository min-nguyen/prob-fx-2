

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}


{- | Non-determinism effect.
-}

module Effects.NonDet (
    NonDet(..)
  , msplit
  , msum
  , msumWeaken
  , branch
  , branchWeaken
  , handleNonDet
  ) where

import Control.Monad ( MonadPlus(..) )
import Control.Applicative ( Alternative((<|>), empty) )
import Prog ( call, discharge, weaken, weakenProg, EffectSum, Member (..), Prog(..) )

-- |  The effect for non-deterministic computations
data NonDet a where
  -- | The operation @Empty@ represents there being no computation branches.
  Empty  :: NonDet a
  -- | The operation @Choose@ is parameterised by a boolean flag, indicating whether to take the left or right computation branch.
  Choose :: NonDet Bool

instance Member NonDet es => Alternative (Prog es) where
  empty :: Member NonDet es => Prog es a
  empty  = call Empty
  (<|>) :: Member NonDet es => Prog es a -> Prog es a -> Prog es a
  m1 <|> m2 = do b <- call Choose
                 if b then m1 else m2

instance Member NonDet es => MonadPlus (Prog es) where
  mzero :: Member NonDet es => Prog es a
  mzero = call Empty
  mplus :: Member NonDet es => Prog es a -> Prog es a -> Prog es a
  mplus m1 m2 = do b <- call Choose
                   if b then m1 else m2

-- | Run a non-determistic program to its next result, and return the rest of the computation
msplit :: forall es a. Member NonDet es => Prog es a -> Prog es (Maybe (a, Prog es a))
msplit m = loop m []
 where
   loop :: Prog es a -> [Prog es a] -> Prog es (Maybe (a, Prog es a))
   loop (Val x) rs   = pure (Just (x, msum rs))
   loop (Op op k) rs = case prj op of
      Just Empty  -> case rs of (r : rs') -> loop r rs'
                                []        -> pure Nothing
      Just Choose -> loop (k True) (k False : rs)
      Nothing     -> Op op (flip loop rs .  k)

-- | Fold a collection of programs into a non-deterministic computation
msum :: (Foldable t, Member NonDet es) => t (Prog es a) -> Prog es a
msum = foldr (<|>) (call Empty)

-- | Weaken then fold a collection of programs into a non-deterministic computation
msumWeaken :: (Functor t, Foldable t) => t (Prog es a) -> Prog (NonDet : es) a
msumWeaken = msum . fmap weakenProg

-- | Branch a program into @n@ computations
branch :: Member NonDet es => Int -> Prog es a -> Prog es a
branch n prog = msum (replicate n prog)

-- | Install the non-determinism effect to a program and then branch into @n@ computations
branchWeaken :: Int -> Prog es a -> Prog (NonDet : es) a
branchWeaken n (Val x)   = msum $ replicate n (Val x)
branchWeaken n (Op op k) = msum $ replicate n (Op (weaken op) (weaken' . k))
  where weaken' (Op op k) = Op (weaken op) (weaken' . k)
        weaken' (Val x)   = Val x

-- | Handle the @NonDet@ effect by running *all* computation branches
handleNonDet :: Prog (NonDet ': es) a -> Prog es [a]
handleNonDet (Val x) = pure [x]
handleNonDet (Op op k) = case discharge op of
   Right Choose -> (<|>) <$> handleNonDet (k True) <*> handleNonDet (k False)
   Right Empty  -> Val []
   Left op  -> Op op (handleNonDet . k)
