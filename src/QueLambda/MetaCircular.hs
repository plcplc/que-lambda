-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.MetaCircular where

import Control.Applicative
import Data.Coerce
import QueLambda.Symantics

data R

-- Somehow this cannot be derived??
instance Applicative (Repr R) where
  pure = coerce
  (<*>) = coerce

deriving via Int instance Num (Repr R Int)

instance Symantics R where
  newtype Repr R a = ReprIdentity {unReprIdentity :: a}
    deriving (Functor, Show)

  int = pure
  bool = pure
  string = pure

  foreach (ReprIdentity ls) f =
    ReprIdentity
      (concatMap (unReprIdentity . f . ReprIdentity) ls)
  where_ (ReprIdentity True) rows = rows
  where_ (ReprIdentity False) _ = pure []

  yield (ReprIdentity x) = pure [x]
  nil = pure []

  (=%) = liftA2 (==)

  and_ (ReprIdentity x) (ReprIdentity y) = pure (x && y)

  newtype Obs R a = ObsIdentity {unObsIdentity :: a}
  observe (ReprIdentity a) = ObsIdentity a

instance SymanticsOpen R where
  lam f = ReprIdentity (unReprIdentity . f . ReprIdentity)
  app = (<*>)

deriving instance Show (GRepr R a b key)

deriving instance Show (GRes R a b key)

deriving instance Show (GBSequence R a)

deriving instance Show (Coll R a b g key)

instance SymanticsG R where
  data ConstK R = ConstKR
  data SumK R = SumKR

  data GRepr R a b key where
    GReprRConst :: Show a => Repr R a -> GRepr R a a (ConstK R) -- Too strong?
    GReprRLessThan ::
      GRepr R Int b1 k1 ->
      GRepr R Int b2 k2 ->
      GRepr R Bool (b1, b2) (k1, k2)
    GReprRPair ::
      (Show a, Show b) =>
      GRepr R a b1 k1 ->
      GRepr R b b2 k2 ->
      GRepr R (a, b) (b1, b2) (k1, k2)
    GReprRSum :: Repr R Int -> GRepr R Int Int (SumK R)

  data GRes R a b key where
    GResRYield :: GRepr R a b key -> GRes R a b key
    GResRHaving ::
      GRepr R Bool b1 k1 ->
      GRes R a b2 k2 ->
      GRes R a (b1, b2) (k1, k2)

  data Coll R a b g k = CollR
    { collGroupKey :: GBSequence R g,
      collGroupRes :: GRes R a b k
    }

  data GBSequence R a where
    GBSequenceROne :: Show a => Repr R a -> GBSequence R (a, ())
    GBSequenceRNext :: Show a => Repr R a -> GBSequence R b -> GBSequence R (a, b)

  data GBKeySequence R a where
    GBKeySequenceRUnit :: GBKeySequence R ()
    GBKeySequenceRNext ::
      GRepr R a a (ConstK R) ->
      GBKeySequence R b ->
      GBKeySequence R (a, b)

  seqOne = GBSequenceROne
  seqNext = GBSequenceRNext

  seqDecon f (GBKeySequenceRNext repr rest) = f repr rest

  group gbSeq k = ReprIdentity [CollR gbSeq $ k (toKeySeq gbSeq)]
    where
      toKeySeq :: GBSequence R gk -> GBKeySequence R gk
      toKeySeq (GBSequenceROne a) =
        GBKeySequenceRNext
          (GReprRConst a)
          GBKeySequenceRUnit
      toKeySeq (GBSequenceRNext a rest) =
        GBKeySequenceRNext (GReprRConst a) (toKeySeq rest)

  gyield = GResRYield
  having = GResRHaving

  gint = GReprRConst . int
  gsum = GReprRSum
  gpair = GReprRPair
  (%>) = GReprRLessThan
