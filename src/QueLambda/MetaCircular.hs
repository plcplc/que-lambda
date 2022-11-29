-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.MetaCircular where

import Control.Applicative
import QueLambda.Symantics
import Data.Coerce

data R

-- Somehow this cannot be derived??
instance Applicative (Repr R) where
  pure = coerce
  (<*>) = coerce

deriving via Int instance Num (Repr R Int)

instance Symantics R where
  newtype Repr R a = ReprIdentity {unReprIdentity :: a}
    deriving (Functor)

  int = pure
  bool = pure
  string = pure

  foreach (ReprIdentity ls) f = ReprIdentity (concatMap (unReprIdentity . f . ReprIdentity) ls)
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
