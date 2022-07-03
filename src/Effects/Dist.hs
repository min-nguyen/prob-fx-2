{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Effects.Dist where

import Data.Map (Map)
import Data.Maybe
import OpenSum (OpenSum)
import PrimDist hiding (prob, sample)
import Prog
import qualified Data.Map as Map
import qualified OpenSum
import Util ( boolToInt )

data Dist a = Dist { getPrimDist :: PrimDist a, getObs :: Maybe a, getTag :: Maybe String}

type Tag  = String
type Addr = (Tag, Int)
type TagMap = Map Tag Int

{- Replaces Dists with Sample or Observe and adds address -}
handleDist :: forall es a. Prog (Dist : es) a -> Prog (Observe : Sample : es) a
handleDist = loop 0 Map.empty
  where
  loop :: Int -> TagMap -> Prog (Dist : es) a -> Prog (Observe : Sample : es) a
  loop _ _ (Val x) = pure x
  loop counter tagMap (Op u k) = case discharge u of
    Right d ->
         case getObs d of
              Just y  -> do call (Observe (getPrimDist d) y (tag, tagIdx)) >>= k'
              Nothing -> do call (Sample (getPrimDist d) (tag, tagIdx))    >>= k'
          where tag     = fromMaybe (show counter) (getTag d)
                tagIdx  = Map.findWithDefault 0 tag tagMap
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop (counter + 1) tagMap' . k
    Left  u'  -> Op (weaken $ weaken u') (loop counter tagMap . k)

data Sample a where
  Sample  :: PrimDist a -> Addr -> Sample a
  Printer :: String -> Sample ()

data Observe a where
  Observe :: PrimDist a -> a -> Addr -> Observe a

printSample :: Member Sample es => String -> Prog es ()
printSample s = Op (inj $ Printer s) Val


instance Show a => Show (Dist a) where
  show (Dist d y tag) = "Dist(" ++ show d ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

instance Eq (Dist a) where
  (==) (Dist d1 _ _) (Dist d2 _ _) = d1 == d2 

pattern PrintPatt :: (Member Sample es) => (x ~ ()) => String -> EffectSum es x
pattern PrintPatt s <- (prj -> Just (Printer s))

pattern Samp :: Member Sample es => PrimDist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj -> Just (Sample d α))

pattern SampPatt :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum es x
pattern SampPatt d α <- (Samp (PrimDistDict d) α)

pattern SampPatt' :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum es x
pattern SampPatt' d α <- (prj -> Just (Sample d@(primDistDict -> Dict) α))

pattern Obs :: Member Observe es => PrimDist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

pattern ObsPatt :: (Member Observe es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum es x
pattern ObsPatt d y α <- (Obs (PrimDistDict d) y α)

pattern DecompLeft :: EffectSum es a -> EffectSum ((:) e es) a
pattern DecompLeft u <- (discharge -> Left u)

pattern DecompRight :: e a -> EffectSum (e : es) a
pattern DecompRight u <- (discharge -> Right u)

{-
isExprInt :: Dist x -> Maybe (Int :~: x)
isExprInt e@(BinomialDist _ _ _ _) = Just Refl
isExprInt _         = Nothing

pattern DistInt :: () => x ~ Int => Dist x
pattern DistInt  <- (isExprInt -> Just Refl)

pattern ExprIntPrj :: Member Dist es => x ~ Int => Dist x -> EffectSum es x
pattern ExprIntPrj e <- (prj -> Just e@DistInt)
-}