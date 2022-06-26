{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effects.Writer where
import Prog

-- | Writer effect and handler
data Writer w a where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) ts => w -> Prog ts ()
tell w = Op (inj $ Tell w) Val

handleWriter :: forall w ts a . Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
handleWriter = loop mempty where
  loop ::  w -> Prog (Writer w ': ts) a -> Prog ts (a, w)
  loop w (Val x) = pure (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

runWriter' :: Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
runWriter'  = handleRelay (\x -> pure (x, mempty))
  (\(Tell w') k -> k () >>= \(x, w) -> pure (x, w' <> w))

runWriter'' :: Monoid w => Prog (Writer w ': ts) a -> Prog ts (a, w)
runWriter'' = handleRelaySt mempty (\w a -> pure (a, w))
  (\w (Tell w') k -> k (w <> w') ())