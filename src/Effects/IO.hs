
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- | For lifting arbitrary monadic computations into an algebraic effect setting.
-}

module Effects.IO (
    random
  , randomFrom
  , liftPrint
  , liftPutStrLn
  , handleIO) where

import Comp ( call, Member(prj), LastMember, Comp(..), Handler, handle )
import Sampler

random :: Member Sampler es => Comp es Double
random = call sampleRandom

randomFrom :: Member Sampler es => [a] -> Comp es a
randomFrom = call . sampleRandomFrom

-- | Wrapper function for calling @Lift@ as the last effect
liftPrint :: Member Sampler es => Show a => a -> Comp es ()
liftPrint = call . liftIO . print

liftPutStrLn :: Member Sampler es => String -> Comp es ()
liftPutStrLn = call . liftIO . putStrLn

handleIO :: Monad m => Comp '[m] w -> m w
handleIO (Val x) = return x
handleIO (Op u q) = case prj u of
  Just m  -> m >>= handleIO . q
  Nothing -> error "Impossible: Nothing cannot occur"