{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Vec (module Vec, Vec.zipWith, Vec.map, Vec.singleton, Vec.fromList, Vec.toList) where

import           Data.Type.Nat
import           Data.Typeable ( Typeable )
import           Data.Vec.Lazy (Vec(..))
import qualified Data.Vec.Lazy as Vec
import           Util ( covariance, variance )

-- | Typeable naturals
class (SNatI n, Typeable n) => TypeableSNatI n

instance (SNatI n, Typeable n) => TypeableSNatI n

-- | Covariance
covar ::            -- Assuming our distribution has D parameters
     [Vec n Double]     -- ^ (f^{1:D})^{1:L}, an L-sized list of D-arity parameter sets  [(D α^1 β^1), (D α^2 β^2), .. (D α^L β^L)]
  -> [Vec n Double]     -- ^ (g^{1:D})^{1:L}, an L-sized list of D-arity parameter sets
  -> Vec n Double       -- ^ covar((f^{1:D})^{1:L}, (g^{1:D})^{1:L})
covar fs gs = Vec.zipWith Util.covariance params_fs params_gs
  where params_fs = transpose fs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]
        params_gs = transpose gs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]

-- | Variance
var ::            -- Assuming our distribution has D parameters
    [Vec n Double]    -- ^ (g^{1:D})^{1:L}, an L-sized list of D-arity parameter sets  [[α^1, β^1], [α^2, β^2], .. [α^L, β^L]]
  -> Vec n Double     -- ^ var((g^{1:D})^{1:L})
var gs = Vec.map Util.variance params_gs
  where params_gs = transpose gs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]

-- | Arithmetic operations
(|+|) :: Vec n Double -> Vec n Double -> Vec n Double
(|+|) = Vec.zipWith (+)

(|-|) :: Vec n Double -> Vec n Double -> Vec n Double
(|-|) = Vec.zipWith (-)

(|*|) :: Vec n Double -> Vec n Double -> Vec n Double
(|*|) = Vec.zipWith (*)

(|/|) :: Vec n Double -> Vec n Double -> Vec n Double
(|/|) = Vec.zipWith (/)

(*|) :: Double -> Vec n Double -> Vec n Double
(*|) x = Vec.map (* x)

-- | Vec utility functions
replicate :: SNat n -> a -> Vec n a
replicate SZ x = VNil
replicate SS x = x ::: Vec.replicate snat x

replicateM :: Monad m => SNat n -> m a -> m (Vec n a)
replicateM n x = sequence $ Vec.replicate n x

iterate :: SNat n -> (t -> t) -> t -> Vec n t
iterate SZ f a = VNil
iterate SS f a = a ::: Vec.iterate snat f (f a)

transpose :: [Vec n a] -> Vec n [a]
transpose (VNil : _) = VNil
transpose vss@((_ ::: _) : _) = Prelude.map Vec.head vss ::: Vec.transpose (Prelude.map Vec.tail vss)
transpose [] = undefined

-- | A linear congruential generator for representing a list of doubles from a single double
linCongGen :: forall n. SNatI n => Double -> SNat n -> Vec n Double
linCongGen r n =
  let ns = Vec.iterate (SS @n) (\n -> ((6364136223846793005*n) + 1442695040888963407) `mod` 2147483647) (decShift r)
  in  Vec.tail $ Vec.map ((/2147483647) . fromIntegral) ns
  where decShift r = floor $ r * 1e16