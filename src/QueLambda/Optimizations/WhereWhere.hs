-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.Optimizations.WhereWhere where

import Data.Kind
import QueLambda.Symantics

data WhereWhere r

deriving via
  LiftedNum (Repr (WhereWhere (r :: Type)) Int)
  instance
    SymanticNum WhereWhere r Int

instance (Symantics s) => LiftRepr (WhereWhere s) s where
  liftRepr = Unknown

instance (Symantics s) => UnliftRepr (WhereWhere s) s where
  unliftRepr = dyn

instance Symantics r => Symantics (WhereWhere r) where
  data Repr (WhereWhere r) a where
    Where :: Repr (WhereWhere r) Bool -> Repr (WhereWhere r) [a] -> Repr (WhereWhere r) [a]
    Unknown :: Repr r a -> Repr (WhereWhere r) a

  {-

     where_ L (where_ M N)

   ~~~>

     where_ (L ∧ M) N

     -}
  where_ cond body =
    case body of
      Where cond' b -> where_ (cond `and_` cond') b
      Unknown {} -> Where cond body

  newtype Obs (WhereWhere r) a = ObsWhereWhere {unObsWhereWhere :: Obs r a}
  observe = ObsWhereWhere . observe . dyn

dyn :: Symantics r => Repr (WhereWhere r) a -> Repr r a
dyn (Unknown u) = u
dyn (Where cond body) = where_ (dyn cond) (dyn body)
