
{-# LANGUAGE FlexibleContexts #-}




{- | State effect.
-}

module Effects.State (
    State(..)
  , get
  , getM
  , put
  , putM
  , modify
  , handleState
  , handleStateM
  , evalState) where

import Comp ( Member(inj), Comp(..), discharge, handleWith )
import Model ( GenModel(..) )

-- | The state effect
data State s a where
  -- | Get the current state
  Get :: State s s
  -- | Set the current state
  Put :: s -> State s ()

-- | Wrapper function for @Get@
get :: (Member (State s) es) => Comp es s
get = Op (inj Get) Val

-- | Wrapper function for @Get@ in a model
getM :: (Member (State s) es) => GenModel env es s
getM = GenModel get

-- | Wrapper function for @Set@
put :: (Member (State s) es) => s -> Comp es ()
put s = Op (inj $ Put s) Val

-- | Wrapper function for @Set@ in a model
putM :: (Member (State s) es) => s -> GenModel env es ()
putM s = GenModel (put s)

-- | Wrapper function for apply a function to the state
modify :: Member (State s) es => (s -> s) -> Comp es ()
modify f = get >>= put . f

-- | Handle the @State s@ effect
handleState :: forall s es a.
  -- | initial state
     s
  -> Comp (State s ': es) a
  -- | (output, final state)
  -> Comp es (a, s)
handleState s = handleWith s (\s' x -> Val (x, s')) hop where
  hop :: forall b. s -> State s b -> (s -> b -> Comp es (a, s)) -> Comp es (a, s)
  hop s Get      k = k s s
  hop _ (Put s') k = k s' ()

evalState :: s -> Comp (State s : es) a -> Comp es a
evalState s = fmap fst . handleState s

-- | Handle the @State s@ effect in a model
handleStateM :: s -> GenModel env (State s : es) a -> GenModel env es (a, s)
handleStateM s m = GenModel $ handleState s $ runModel m
