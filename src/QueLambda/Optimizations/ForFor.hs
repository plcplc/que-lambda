{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module QueLambda.Optimizations.ForFor where

import QueLambda.Symantics

data ForFor r

instance (Symantics r) => Num (Repr (ForFor r) Int) where
  (+) x y = Unknown ((+) (dyn x) (dyn y))
  (*) x y = Unknown ((*) (dyn x) (dyn y))
  abs = Unknown . abs . dyn
  signum = Unknown . signum . dyn
  fromInteger = dyn . fromInteger
  negate = Unknown . negate . dyn

instance (Symantics s) => LiftRepr (ForFor s) s where
  liftRepr = Unknown

instance (Symantics s) => UnliftRepr (ForFor s) s where
  unliftRepr = dyn

{-
I can't get this working :-/
deriving via
  LiftedNum (Repr (ForFor r) Int)
  instance Num (Repr (ForFor r) Int)
  -}

instance (Symantics r) => Symantics (ForFor r) where
  data Repr (ForFor r) a where
    ForEach :: Repr (ForFor r) [a] -> (Repr (ForFor r) a -> Repr (ForFor r) [b]) -> Repr (ForFor r) [b]
    Unknown :: Repr r a -> Repr (ForFor r) a

  {-

     foreach (foreach L (\y -> M)) (\x -> N)

   ~~~>

     foreach L (\y -> foreach M (\x -> N))

     -}
  foreach s _N = case s of
    Unknown {} -> ForEach s _N
    ForEach _L _M ->
      Unknown $ foreach (dyn _L) (\y -> foreach (dyn $ _M $ Unknown y) (dyn . _N . Unknown))

  newtype Obs (ForFor r) a = ObsForFor {unObsForFor :: Obs r a}
  observe = ObsForFor . observe . dyn

dyn :: Symantics r => Repr (ForFor r) a -> Repr r a
dyn (Unknown u) = u
dyn (ForEach f b) = foreach (dyn f) (dyn . b . Unknown)
