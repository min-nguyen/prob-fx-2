
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- | For lifting arbitrary monadic computations into an algebraic effect setting.
-}

module Effects.Lift (
    HasSampler
  , random'
  , randomFrom'
  , liftPrint
  , liftPutStrLn
  , handleM) where

import Prog ( call, Member(prj), LastMember, Prog(..) )
import Sampler

type HasSampler es = LastMember Sampler es

random' :: HasSampler es => Prog es Double
random' = call sampleRandom

randomFrom' :: HasSampler es => [a] -> Prog es a
randomFrom' = call . sampleRandomFrom

-- | Wrapper function for calling @Lift@ as the last effect
liftPrint :: LastMember Sampler es => Show a => a -> Prog es ()
liftPrint = call . liftIO . print

liftPutStrLn :: LastMember Sampler es => String -> Prog es ()
liftPutStrLn = call . liftIO . putStrLn

-- | Handle @Lift m@ as the last effect
handleM :: Monad m => Prog '[m] w -> m w
handleM (Val x) = return x
handleM (Op u q) = case prj u of
     Just m  -> m >>= handleM . q
     Nothing -> error "Impossible: Nothing cannot occur"

