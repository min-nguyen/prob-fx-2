{-# LANGUAGE DerivingVia #-}

module Node where

newtype Node a = Node ([String], a)
  deriving
    stock (Eq, Ord, Show, Read)

instance Semigroup a => Semigroup (Node a) where
  Node (xs, a) <> Node (ys, b) = Node (xs <> ys, a <> b)

instance Monoid a => Monoid (Node a) where
  mempty = Node ([], mempty)

instance Num a => Num (Node a) where
  Node (xs, n) + Node (ys, m)   = Node (xs <> ys, n + m)
  Node (xs, n) * Node (ys, m)   = Node (xs <> ys, n * m)
  Node (xs, n) - (Node (ys, m)) = Node (xs <> ys, n - m)
  abs (Node (xs, n))            = Node (xs, abs n)
  signum (Node (xs, n))         = Node (xs, signum n)
  fromInteger n                 = Node (mempty, fromInteger n)

instance Fractional a => Fractional (Node a) where
  Node (xs, n) / Node (ys, m)   = Node (xs <> ys, n / m)
  fromRational n                = Node (mempty, fromRational n)
  recip (Node (xs, n))          = Node (xs, recip n)

-- (*) :: (Num a, Num b) => a -> b -> a