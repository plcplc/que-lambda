{-# LANGUAGE QuantifiedConstraints #-}
-- See https://okmij.org/ftp/meta-programming/quel.pdf
{-# LANGUAGE UndecidableInstances #-}

module QueLambda.Symantics where

import Data.Kind

class Symantics s => LiftRepr (r :: Type) s | r -> s where
  liftRepr :: Repr s a -> Repr r a

class Symantics s => UnliftRepr (r :: Type) s | r -> s where
  unliftRepr :: Repr r a -> Repr s a

-- | Type alias to relieve the deriving clause from the somewhat convoluted
-- incantation of the instance context.
type SymanticNum :: (Type -> Type) -> Type -> Type -> Constraint
type SymanticNum r s a =
  ( Symantics s,
    Num (Repr s a)
  ) =>
  Num (Repr (r s) a)

-- | Newtype to drive DerivingVia for deriving 'Num' instances.
newtype LiftedNum a = LiftedNum {unliftedNum :: a}

-- | 'Num' instance, which is suitable to use with 'DerivingVia':
--
-- For some concrete type R where 'Symantics s => Symantics (R s)':
--
--    deriving via
--      LiftedNum (Repr (R (s :: Type)) Int)
--      instance
--        SymanticNum R s Int
--
-- Note that, without the kind signature '(s :: Type), this fails in GHC 9.2.4
-- with the dreaded "No skolem info" error, see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/22302.
instance
  ( UnliftRepr (r s) s,
    LiftRepr (r s) s,
    Num (Repr s a)
  ) =>
  Num (LiftedNum (Repr (r s) a))
  where
  (+) x y = LiftedNum $ liftRepr ((+) (unliftRepr (unliftedNum x)) (unliftRepr (unliftedNum y)))
  (*) x y = LiftedNum $ liftRepr ((*) (unliftRepr (unliftedNum x)) (unliftRepr (unliftedNum y)))
  abs x = LiftedNum $ liftRepr $ abs (unliftRepr $ unliftedNum x)
  signum = LiftedNum . liftRepr . signum . unliftRepr . unliftedNum
  fromInteger = LiftedNum . liftRepr . fromInteger
  negate = LiftedNum . liftRepr . negate . unliftRepr . unliftedNum

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

  and_ :: Repr r Bool -> Repr r Bool -> Repr r Bool
  default and_ :: (LiftRepr r s, UnliftRepr r s) => Repr r Bool -> Repr r Bool -> Repr r Bool
  and_ x y = liftRepr $ and_ (unliftRepr x) (unliftRepr y)

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

class Symantics r => SymanticsG r where
  data ConstK r :: Type
  data SumK r :: Type

  data GRepr r :: Type -> Type -> Type
  data GRes r :: Type -> Type -> Type

  data Coll r :: Type -> Type -> Type -> Type

  data GBSequence r :: Type -> Type
  data GBKeySequence r :: Type -> Type

  seqOne :: Show a => Repr r a -> GBSequence r (a, ())

  seqNext :: Show a => Repr r a -> GBSequence r b -> GBSequence r (a, b)

  seqDecon ::
    ( GRepr r a (ConstK r) ->
      GBKeySequence r b ->
      w
    ) ->
    GBKeySequence r (a, b) ->
    w

  group ::
    GBSequence r gk ->
    (GBKeySequence r gk -> GRes r a res) ->
    Repr r [Coll r a gk res]

  having ::
    GRepr r Bool k1 -> GRes r a k2 -> GRes r a (k1, k2)

  gyield ::
    GRepr r a key -> GRes r a key

  gint :: Int -> GRepr r Int (ConstK r)

  gsum :: Repr r Int -> GRepr r Int (SumK r)

  gpair :: (Show a, Show b) => GRepr r a k1 -> GRepr r b k2 -> GRepr r (a, b) (k1, k2)

  (%>) :: GRepr r Int k1 -> GRepr r Int k2 -> GRepr r Bool (k1, k2)
