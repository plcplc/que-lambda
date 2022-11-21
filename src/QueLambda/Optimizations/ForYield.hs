module QueLambda.Optimizations.ForYield where

import QueLambda.Symantics

data ForYield r

instance (Symantics r) => Num (Repr (ForYield r) Int) where
  (+) x y = Unknown ((+) (dyn x) (dyn y))
  (*) x y = Unknown ((*) (dyn x) (dyn y))
  abs = Unknown . abs . dyn
  signum = Unknown . signum . dyn
  fromInteger = dyn . fromInteger
  negate = Unknown . negate . dyn

instance (Symantics s) => LiftRepr (ForYield s) s where
  liftRepr = Unknown

instance (Symantics s) => UnliftRepr (ForYield s) s where
  unliftRepr = dyn

{-
I can't get this working :-/
deriving via
  LiftedNum (Repr (ForYield r) Int)
  instance Num (Repr (ForYield r) Int)
  -}

instance (Symantics r) => Symantics (ForYield r) where
  data Repr (ForYield r) a where
    ForEach :: Repr (ForYield r) [a] -> (Repr (ForYield r) a -> Repr (ForYield r) [b]) -> Repr (ForYield r) [b]
    Yield :: Repr (ForYield r) a -> Repr (ForYield r) [a]
    Unknown :: Repr r a -> Repr (ForYield r) a

  {-

     foreach (yield M) (\x -> N)

   ~~~>

     M[x := N]

     -}
  foreach s _N = case s of
    Yield l -> _N l
    _ -> ForEach s _N

  yield = Yield

  newtype Obs (ForYield r) a = ObsForYield {unObsForYield :: Obs r a}
  observe = ObsForYield . observe . dyn

dyn :: Symantics r => Repr (ForYield r) a -> Repr r a
dyn (Unknown u) = u
dyn (ForEach f b) = foreach (dyn f) (dyn . b . Unknown)
dyn (Yield v) = yield (dyn v)
