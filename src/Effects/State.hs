{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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

import Prog ( Member(inj), Prog(..), discharge, handle )
import Model ( Model(..) )

-- | The state effect
data State s a where
  -- | Get the current state
  Get :: State s s
  -- | Set the current state
  Put :: s -> State s ()

-- | Wrapper function for @Get@
get :: (Member (State s) es) => Prog es s
get = Op (inj Get) Val

-- | Wrapper function for @Get@ in a model
getM :: (Member (State s) es) => Model env es s
getM = Model get

-- | Wrapper function for @Set@
put :: (Member (State s) es) => s -> Prog es ()
put s = Op (inj $ Put s) Val

-- | Wrapper function for @Set@ in a model
putM :: (Member (State s) es) => s -> Model env es ()
putM s = Model (put s)

-- | Wrapper function for apply a function to the state
modify :: Member (State s) es => (s -> s) -> Prog es ()
modify f = get >>= put . f

-- | Handle the @State s@ effect
handleState ::
  -- | initial state
     s
  -> Prog (State s ': es) a
  -- | (output, final state)
  -> Prog es (a, s)
handleState s m = loop s m where
  loop :: s -> Prog (State s ': es) a -> Prog es (a, s)
  loop s (Val x) = return (x, s)
  loop s (Op u k) = case discharge u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'         -> Op u' (loop s . k)

evalState :: s -> Prog (State s : es) a -> Prog es a
evalState s = fmap fst . handleState s

-- | Handle the @State s@ effect in a model
handleStateM :: s -> Model env (State s : es) a -> Model env es (a, s)
handleStateM s m = Model $ handleState s $ runModel m
