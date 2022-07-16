{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


{- | The state effect
-}

module Effects.State (
    State(..)
  , get 
  , getM
  , put
  , putM
  , modify
  , handleState
  , handleStateM) where

import Prog ( Member(inj), Prog(..), discharge )
import Model ( Model(..) ) 

-- | The state effect
data State s a where
  Get :: State s s
  Put :: s -> State s ()

-- | Get the state
get :: (Member (State s) es) => Prog es s
get = Op (inj Get) Val

-- | Get the state in a model
getM :: (Member (State s) es) => Model env es s
getM = Model get

-- | Set the state
put :: (Member (State s) es) => s -> Prog es ()
put s = Op (inj $ Put s) Val

-- | Set the state in a model
putM :: (Member (State s) es) => s -> Model env es ()
putM s = Model (put s)

-- | Apply a function to the state
modify :: Member (State s) es => (s -> s) -> Prog es ()
modify f = get >>= put . f

-- | Handle the @State s@ effect
handleState :: 
  -- | Initial state
     s 
  -- | Initial program
  -> Prog (State s ': es) a 
  -- | Pure value and final state
  -> Prog es (a, s)
handleState s m = loop s m where
  loop :: s -> Prog (State s ': es) a -> Prog es (a, s)
  loop s (Val x) = return (x, s)
  loop s (Op u k) = case discharge u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'         -> Op u' (loop s . k)

-- | Handle the @State s@ effect in a model
handleStateM :: s -> Model env (State s : es) a -> Model env es (a, s)
handleStateM s m = Model $ handleState s $ runModel m
