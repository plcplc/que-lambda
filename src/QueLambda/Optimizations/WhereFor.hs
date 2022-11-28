-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.Optimizations.WhereFor where

import QueLambda.Symantics

data WhereFor r

-- We really should be able to use deriving-via for this :-/
instance (Symantics r) => Num (Repr (WhereFor r) Int) where
  (+) x y = Unknown ((+) (dyn x) (dyn y))
  (*) x y = Unknown ((*) (dyn x) (dyn y))
  abs = Unknown . abs . dyn
  signum = Unknown . signum . dyn
  fromInteger = dyn . fromInteger
  negate = Unknown . negate . dyn

instance (Symantics s) => LiftRepr (WhereFor s) s where
  liftRepr = Unknown

instance (Symantics s) => UnliftRepr (WhereFor s) s where
  unliftRepr = dyn

instance Symantics r => Symantics (WhereFor r) where
  data Repr (WhereFor r) a where
    ForEach :: Repr (WhereFor r) [a] -> (Repr (WhereFor r) a -> Repr (WhereFor r) [b]) -> Repr (WhereFor r) [b]
    Unknown :: Repr r a -> Repr (WhereFor r) a

  foreach = ForEach

  {-

     where_ L (foreach M) (\x -> N)

   ~~~>

     (foreach M (\x -> where_ L N))

     -}
  where_ cond body =
    case body of
      ForEach t b -> ForEach t (where_ cond . b)
      Unknown u -> Unknown (where_ (dyn cond) u)

  newtype Obs (WhereFor r) a = ObsWhereFor {unObsWhereFor :: Obs r a}
  observe = ObsWhereFor . observe . dyn

dyn :: Symantics r => Repr (WhereFor r) a -> Repr r a
dyn (Unknown u) = u
dyn (ForEach t b) = foreach (dyn t) (dyn . b . Unknown)
