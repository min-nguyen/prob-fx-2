
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- | For lifting arbitrary monadic computations into an algebraic effect setting.
-}

module Effects.Lift (
    HasSampler
  , lift
  , random'
  , randomFrom'
  , liftPrint
  , liftPutStrLn
  , handleM) where

import Prog ( call, Member(prj), LastMember, Prog(..) )
import Sampler

type HasSampler es = LastMember Sampler es

random' :: HasSampler es => Prog es Double
random' = lift sampleRandom

randomFrom' :: HasSampler es => [a] -> Prog es a
randomFrom' = lift . sampleRandomFrom

-- | Wrapper function for calling @Lift@ as the last effect
lift :: LastMember m es => m a -> Prog es a
lift = call

liftPrint :: LastMember Sampler es => Show a => a -> Prog es ()
liftPrint = lift . liftIO . print

liftPutStrLn :: LastMember Sampler es => String -> Prog es ()
liftPutStrLn = lift . liftIO . putStrLn

-- | Handle @Lift m@ as the last effect
handleM :: Monad m => Prog '[m] w -> m w
handleM (Val x) = return x
handleM (Op u q) = case prj u of
     Just m  -> m >>= handleM . q
     Nothing -> error "Impossible: Nothing cannot occur"

