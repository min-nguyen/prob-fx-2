{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Prog where

import Control.Monad
import Unsafe.Coerce
import Data.Kind (Constraint)
import GHC.TypeLits
    ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import Data.Typeable
-- import qualified OpenSum as OpenSum
import FindElem

{- Effect Sum -}
data EffectSum (es :: [* -> *]) (x :: *) :: * where
  EffectSum :: Int -> e x -> EffectSum es x

class (FindElem e es) => Member (e :: * -> *) (es :: [* -> *]) where
  inj ::  e x -> EffectSum es x
  prj ::  EffectSum es x -> Maybe (e x)

instance {-# INCOHERENT #-} (e ~ e') => Member e '[e'] where
   inj x          = EffectSum 0 x
   prj (EffectSum _ x) = Just (unsafeCoerce x)

instance (FindElem e es) => Member e es where
  inj = inj' (unP (findElem :: P e es))
  prj = prj' (unP (findElem :: P e es))

-- | Not possible to implement "Members" as a type class.
-- class Members e (tss :: [* -> *])
-- instance (Member e tss, Members es tss) => Members (e ': es) tss
-- instance Members '[] es

type family Members (es :: [* -> *]) (tss :: [* -> *]) = (cs :: Constraint) | cs -> es where
  Members (e ': es) tss = (Member e tss, Members es tss)
  Members '[] tss       = ()

pattern Other :: EffectSum es x -> EffectSum  (e ': es) x
pattern Other u <- (discharge -> Left u)

inj' :: Int -> e x -> EffectSum es x
inj' = EffectSum

prj' :: Int -> EffectSum es x -> Maybe (e x)
prj' n (EffectSum n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

discharge :: EffectSum (e ': es) x -> Either (EffectSum es x) (e x)
discharge (EffectSum 0 tv) = Right $ unsafeCoerce tv
discharge (EffectSum n rv) = Left  $ EffectSum (n-1) rv

weaken :: EffectSum es a -> EffectSum (any ': es) a
weaken (EffectSum n ta) = EffectSum (n + 1) ta

weaken' :: Prog es a -> Prog (any : es) a
weaken' (Op op k) = Op (weaken op) (weaken' . k)
weaken' (Val x)   = Val x

infixr 5 :++:
type family xs :++: ys where
  '[] :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

class Weakens e where
  weakens :: EffectSum es a -> EffectSum (e :++: es) a
instance Weakens '[] where
  weakens = id
instance Weakens es => Weakens (e ': es) where
  weakens u = weaken (weakens @es u)

-- | Unique Member
type family UMember (b :: Bool) (e :: * -> *) (es :: [* -> *]) :: Bool where
  UMember 'True e (e ': es)   = 'False
  UMember 'True e (e' ': es)  = UMember 'True e es
  UMember 'True e '[]         = 'True
  UMember 'False e (e ': es)  = UMember 'True e es
  UMember 'False e (e' ': es) = UMember 'False e es
  UMember 'False e '[]        = 'False

class    (UMember 'False e es ~ True, Member e es) => UniqueMember e es
instance (UMember 'False e es ~ True, Member e es) => UniqueMember e es

-- | Last effect in effect list
class Member m effs => LastMember m effs | effs -> m
instance {-# OVERLAPPABLE #-} LastMember m effs => LastMember m (eff ': effs)
instance LastMember m (m ': '[])

-- | Prog monad
data Prog es a where
  Val :: a -> Prog es a
  Op  :: EffectSum es x -> (x -> Prog es a) -> Prog es a

instance Functor (Prog es) where
  fmap f (Val a) = Val (f a)
  fmap f (Op fx k) = Op fx (fmap f . k)

instance Applicative (Prog es) where
  pure = Val
  Val f <*> x = fmap f x
  (Op fx k) <*> x = Op fx ((<*> x) . k)

instance Monad (Prog es) where
  Val a >>= f      = f a
  Op fx k >>= f = Op fx (k >=> f)

run :: Prog '[] a -> a
run (Val x) = x
run _ = error "'run' isn't defined for non-pure computations"

runM :: forall m a es. (Monad m, LastMember m es) => Prog es a -> m a
runM (Val x) = pure x
runM (Op u k) =
  let x = prj @m u
  in case x of
      Just mb -> mb >>= runM . k
      Nothing -> error "Impossible: Nothing cannot occur"

-- call :: (UniqueMember e es, Member e es) => e x -> Prog es x
call :: (Member e es) => e x -> Prog es x
call e = Op (inj e) Val

sendM :: (Monad m, LastMember m es) => m a -> Prog es a
sendM = call

-- | Given request, handle or relay it, and discharge it from the list of effects
handleRelay ::
     (a -> Prog es b)                                  -- Return handler
  -> (forall x. e x -> (x -> Prog es b) -> Prog es b) -- Request handler
  -> Prog (e ': es) a
  -> Prog es b
handleRelay ret _ (Val x) = ret x
handleRelay ret h (Op u k) =
  case discharge u of
    Right x  -> h x (handleRelay ret h . k)
    Left  u' -> Op u' (handleRelay ret h . k)

-- | Parameterised version of handleRelay to allow a state to be managed
handleRelaySt ::
     s                                                            -- State
  -> (s -> a -> Prog es b)                                       -- Return handler
  -> (forall x. s -> e x -> (s -> x -> Prog es b) -> Prog es b) -- Request handler
  -> Prog (e ': es) a
  -> Prog es b
handleRelaySt s ret _ (Val x) = ret s x
handleRelaySt s ret h (Op u k) =
  case discharge u of
    Right tx -> h s tx (\s' x -> handleRelaySt s' ret h $ k x)
    Left u' -> Op u' (handleRelaySt s ret h . k)

-- | Intercept and handle requests from program, but do not discharge effect from type-level
--   Requires type application on usage to specify which effect is being intercepted, e.g. "interpose @(Reader Int)"
interpose :: Member e es =>
     (a -> Prog es b)
  -> (forall x. e x -> (x -> Prog es b) -> Prog es b)
  -> Prog es a
  -> Prog es b
interpose ret h (Val x ) = ret x
interpose ret h (Op u k) =
  case prj u  of
    Just tx -> h tx (interpose ret h . k)
    Nothing -> Op u (interpose ret h . k)

interposeSt :: Member e es =>
     s
  -> (s -> a -> Prog es b)
  -> (forall x. s -> e x -> (s -> x -> Prog es b) -> Prog es b)
  -> Prog es a
  -> Prog es b
interposeSt s ret h (Val x ) = ret s x
interposeSt s ret h (Op u k) =
  case prj u  of
    Just tx -> h s tx (\s' x -> interposeSt s' ret h $ k x)
    Nothing -> Op u (interposeSt s ret h . k)

-- | Replace the effect e at the front of the list of effects with a new effect e.
replaceRelay ::
      (a -> Prog (e ': es) b)
  ->  (forall x. e x -> (x -> Prog (e ': es) b) -> Prog (e ': es) b)
  ->  Prog (e ': es) a
  ->  Prog (e ': es) b
replaceRelay ret h (Val x) = ret x
replaceRelay ret h (Op u k) = case discharge u of
  Right tx -> h tx (replaceRelay ret h . k)
  Left  u' -> Op (weaken u') (replaceRelay ret h . k)

replaceRelayN :: forall rs e es a b . Weakens rs =>
      (a -> Prog (rs :++: es) b)
  ->  (forall x. e x -> (x -> Prog (rs :++: es) b) -> Prog (rs :++: es) b)
  ->  Prog (e ': es) a
  ->  Prog (rs :++: es) b
replaceRelayN ret h (Val x) = ret x
replaceRelayN ret h (Op u k) = case discharge u of
  Right tx -> h tx (replaceRelayN @rs ret h . k)
  Left  u' -> Op (weakens @rs u') (replaceRelayN @rs ret h . k)

replaceRelaySt ::
      s
  ->  (s -> a -> Prog (e ': es) b)
  ->  (forall x. s -> e x -> (s -> x -> Prog (e ': es) b) -> Prog (e ': es) b)
  ->  Prog (e ': es) a
  ->  Prog (e ': es) b
replaceRelaySt s ret h (Val x) = ret s x
replaceRelaySt s ret h (Op u k) = case discharge u of
  Right tx -> h s tx (\s' x -> replaceRelaySt s' ret h $ k x)
  Left  u' -> Op (weaken u') (replaceRelaySt s ret h . k)

replaceRelayStN :: forall rs es s e a b . Weakens rs =>
      s
  ->  (s -> a -> Prog (rs :++: es) b)
  ->  (forall x. s -> e x -> (s -> x -> Prog (rs :++:  es) b) -> Prog (rs :++: es) b)
  ->  Prog (e ': es) a
  ->  Prog (rs :++: es) b
replaceRelayStN s ret h (Val x) = ret s x
replaceRelayStN s ret h (Op u k) = case discharge u of
  Right tx -> h s tx (\s' x -> replaceRelayStN @rs s' ret h $ k x)
  Left  u' -> Op (weakens @rs u') (replaceRelayStN @rs s ret h . k)


-- | Find some existing effect e in es, leave it unhandled, and install new effect e' after every request of e. This adds the effect e' to the front of es.
-- Requires type application on usage to specify which effect is being intercepted, e.g. "installPrepend @(Reader Int)"
install :: Member e es =>
     (a -> Prog (e' ': es) a)
  -> (forall x. x -> e x -> (x -> Prog (e' ': es) a) -> Prog (e' ': es) a)
  -> Prog es a
  -> Prog (e' ': es) a
install ret h (Val x )  = ret x
install ret h (Op u k) =
  case prj u  of
    Just tx -> Op (weaken u) (\x -> h x tx (install ret h . k))
    Nothing -> Op (weaken u) (install ret h . k)

installN :: forall rs es e a . (Member e es, Weakens rs) =>
     (a -> Prog (rs :++: es) a)
  -> (forall x. x -> e x -> (x -> Prog (rs :++: es) a) -> Prog (rs :++: es) a)
  -> Prog es a
  -> Prog (rs :++: es) a
installN ret h (Val x )  = ret x
installN ret h (Op u k) =
  case prj u  of
    Just tx -> Op (weakens @rs u) (\x -> h x tx (installN @rs ret h . k))
    Nothing -> Op (weakens @rs u) (installN @rs ret h . k)

-- | Get effect e from (e ': es), leave it unhandled, and install new effect e' after every request of e. This adds e' to the front of (e ': es).
installFront ::
     (a -> Prog (e' ': e ': es) a)
  -> (forall x. x -> e x -> (x -> Prog (e' ': e ': es) a) -> Prog (e' ': e ': es) a)
  -> Prog (e ': es) a
  -> Prog (e' ': e ': es) a
installFront ret h (Val x )  = ret x
installFront ret h (Op u k) =
  case prj u  of
    Just tx -> Op (weaken u) (\x -> h x tx (installFront ret h . k))
    Nothing -> Op (weaken u) (installFront ret h . k)

-- | Find some effect e in es, leave it unhandled, and inject operation for another existing effect e' in es.
-- Requires type application on usage to specify which effect is being intercepted and which is being inserted, e.g. "installPrepend @(Reader Int) @(Writer [Int])"
installExisting :: forall e e' es a b. (Member e es, Member e' es) =>
     (a -> Prog es b)
  -> (forall x. x -> e x -> (x -> Prog es b) -> Prog es b)
  -> Prog es a
  -> Prog es b
installExisting ret h (Val x )  = ret x
installExisting ret h (Op u k) =
  case prj u  of
    Just tx -> Op u (\x -> h x tx (installExisting @e @e' ret h . k))
    Nothing -> Op u (installExisting @e @e' ret h . k)


data Free f a = Val' a | Op' (f (Free f a))

instance Functor f => Functor (Free f) where
   fmap g (Op' fx) = Op' (fmap g <$> fx)
   fmap g (Val' x)  = Val' (g x)

instance (Functor f) => Applicative (Free f) where
  pure = Val'
  Val' f  <*> as  = fmap f as
  Op' faf  <*> as  = Op' (fmap (<*> as) faf)

instance (Functor f) => Monad (Free f) where
  (Op' x) >>= f = Op' (fmap (>>= f) x)
  (Val' es) >>= f = f es

data Reader' env a where
  Ask' :: (env -> a) -> Reader' env a
  deriving Functor

call' e = Val' (e Val')

prog :: Free (Reader' Int) ()
prog = do
  call' Ask'
  pure ()

runProg :: Free (Reader' Int) () -> IO ()
runProg (Op' (Ask' k)) = do
  runProg (k (4 :: Int))