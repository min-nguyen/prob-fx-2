
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
handleState ::
  -- | initial state
     s
  -> Comp (State s ': es) a
  -- | (output, final state)
  -> Comp es (a, s)
handleState s m = loop s m where
  loop :: s -> Comp (State s ': es) a -> Comp es (a, s)
  loop s (Val x) = return (x, s)
  loop s (Op u k) = case discharge u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'         -> Op u' (loop s . k)

evalState :: s -> Comp (State s : es) a -> Comp es a
evalState s = fmap fst . handleState s

-- | Handle the @State s@ effect in a model
handleStateM :: s -> GenModel env (State s : es) a -> GenModel env es (a, s)
handleStateM s m = GenModel $ handleState s $ runGenModel m
