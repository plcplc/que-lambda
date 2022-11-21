-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.Optimizations.ForWhere1 where

import QueLambda.Symantics

data ForWhere1 r

-- We really should be able to use deriving-via for this :-/
instance (Symantics r) => Num (Repr (ForWhere1 r) Int) where
  (+) x y = Unknown ((+) (dyn x) (dyn y))
  (*) x y = Unknown ((*) (dyn x) (dyn y))
  abs = Unknown . abs . dyn
  signum = Unknown . signum . dyn
  fromInteger = dyn . fromInteger
  negate = Unknown . negate . dyn

instance (Symantics s) => LiftRepr (ForWhere1 s) s where
  liftRepr = Unknown

instance (Symantics s) => UnliftRepr (ForWhere1 s) s where
  unliftRepr = dyn

instance Symantics r => Symantics (ForWhere1 r) where
  data Repr (ForWhere1 r) a where
    Where :: Repr (ForWhere1 r) Bool -> Repr (ForWhere1 r) [a] -> Repr (ForWhere1 r) [a]
    Unknown :: Repr r a -> Repr (ForWhere1 r) a

  {-

     foreach (where_ L M) (\x -> N)

   ~~~>

     where_ L (foreach M (\x -> N))

     -}
  foreach s _N = case s of
    Unknown {} -> Unknown $ foreach (dyn s) (dyn . _N . Unknown)
    Where _L _M ->
      Unknown $
        where_
          (dyn _L)
          ( foreach
              (dyn _M)
              (dyn . _N . Unknown)
          )

  where_ = Where

  newtype Obs (ForWhere1 r) a = ObsForWhere1 {unObsForWhere1 :: Obs r a}
  observe = ObsForWhere1 . observe . dyn


dyn :: Symantics r => Repr (ForWhere1 r) a -> Repr r a
dyn (Unknown u) = u
dyn (Where c b) = where_ (dyn c) (dyn b)
