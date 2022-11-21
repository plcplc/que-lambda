-- See https://okmij.org/ftp/meta-programming/quel.pdf
{-# LANGUAGE UndecidableInstances #-}

module QueLambda.Symantics where

import Data.Kind

class Symantics s => LiftRepr (r :: Type) s | r -> s where
  liftRepr :: Repr s a -> Repr r a

class Symantics s => UnliftRepr (r :: Type) s | r -> s where
  unliftRepr :: Repr r a -> Repr s a

newtype LiftedNum r = LiftedNum {unliftedNum :: r}

instance
  ( UnliftRepr (r s) s,
    LiftRepr (r s) s,
    Num (Repr s Int)
  ) =>
  Num (LiftedNum (Repr (r s) Int))
  where
  (+) x y = LiftedNum $ liftRepr ((+) (unliftRepr (unliftedNum x)) (unliftRepr (unliftedNum y)))
  (*) x y = LiftedNum $ liftRepr ((*) (unliftRepr (unliftedNum x)) (unliftRepr (unliftedNum y)))
  abs x = LiftedNum $ liftRepr $ abs (unliftRepr $ unliftedNum x)
  signum = LiftedNum . liftRepr . signum . unliftRepr. unliftedNum
  fromInteger = LiftedNum . liftRepr . fromInteger
  negate = LiftedNum . liftRepr .  negate . unliftRepr . unliftedNum

class (Num (Repr r Int)) => Symantics (r :: Type) where
  data Repr r :: Type -> Type

  -- TODO: Delegate to separate type class.
  int :: Int -> Repr r Int
  default int :: LiftRepr r s => Int -> Repr r Int
  int = liftRepr . int

  bool :: Bool -> Repr r Bool
  default bool :: LiftRepr r s => Bool -> Repr r Bool
  bool = liftRepr . bool

  string :: String -> Repr r String
  default string :: LiftRepr r s => String -> Repr r String
  string = liftRepr . string

  foreach :: Repr r [a] -> (Repr r a -> Repr r [b]) -> Repr r [b]
  default foreach ::
    (UnliftRepr r s, LiftRepr r s) =>
    Repr r [a] ->
    (Repr r a -> Repr r [b]) ->
    Repr r [b]
  foreach t b = liftRepr (foreach (unliftRepr t) (unliftRepr . b . liftRepr))

  where_ :: Repr r Bool -> Repr r [a] -> Repr r [a]
  default where_ ::
    (UnliftRepr r s, LiftRepr r s) =>
    Repr r Bool ->
    Repr r [a] ->
    Repr r [a]
  where_ c b = liftRepr (where_ (unliftRepr c) (unliftRepr b))

  yield :: Repr r a -> Repr r [a]
  default yield :: (UnliftRepr r s, LiftRepr r s) => Repr r a -> Repr r [a]
  yield = liftRepr . yield . unliftRepr

  nil :: Repr r [a]
  default nil :: LiftRepr r s => Repr r [a]
  nil = liftRepr nil

  (=%) :: (Eq a) => Repr r a -> Repr r a -> Repr r Bool
  -- ^
  -- Q: 'Eq a' required for meta-circular interpreter.
  --    How can we avoid that?
  default (=%) :: (UnliftRepr r s, LiftRepr r s, Eq a) => Repr r a -> Repr r a -> Repr r Bool
  (=%) x y = liftRepr ((=%) (unliftRepr x) (unliftRepr y))

  {- Omitting numerical ops, set union, and loads more -}

  data Obs r :: Type -> Type
  observe :: Repr r a -> Obs r a

-- The SQL that we target don't actually support lambdas, and having them part
-- of 'Symantics' would be awkward, as we would have to check that they can get
-- applied-away. A real mess. And they're not essential for the purpose of
-- writing queries.
--
-- So now they're delegated to their own type-class.
class Symantics r => SymanticsOpen r where
  lam :: (Repr r a -> Repr r b) -> Repr r (a -> b)
  app :: Repr r (a -> b) -> Repr r a -> Repr r b
