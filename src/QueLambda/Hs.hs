-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.Hs where

import Control.Monad.State
import Data.Coerce
import Data.List
import QueLambda.Symantics

data HsGen

instance Num (Repr HsGen Int) where
  (+) x y = coerce $ HsOp "+" (coerce x) (coerce y)
  (*) x y = coerce $ HsOp "*" (coerce x) (coerce y)
  abs x = coerce $ HsFnApp "abs" [coerce x]
  signum x = coerce $ HsFnApp "signum" [coerce x]
  fromInteger = int . fromInteger
  negate x = coerce $ HsFnApp "negate" [coerce x]

data HsQuote where
  HsLit :: String -> HsQuote
  HsVar :: String -> HsQuote
  HsForEach :: HsQuote -> (HsQuote -> HsQuote) -> HsQuote
  HsWhere :: HsQuote -> HsQuote -> HsQuote
  HsYield :: HsQuote -> HsQuote
  HsNil :: HsQuote
  HsOp :: String -> HsQuote -> HsQuote -> HsQuote
  HsFnApp :: String -> [HsQuote] -> HsQuote
  HsTable :: String -> HsQuote
  HsMkBag :: [(String, HsQuote)] -> HsQuote
  HsProjBag :: HsQuote -> String -> HsQuote

instance Symantics HsGen where
  newtype Repr HsGen a = ReprHsGen {unReprHsGen :: HsQuote}

  int = ReprHsGen . HsLit . show
  bool = ReprHsGen . HsLit . show
  string = ReprHsGen . HsLit . show

  foreach t b = coerce $ HsForEach (coerce t) (coerce b)
  where_ c b = coerce $ HsWhere (coerce c) (coerce b)

  yield = coerce . HsYield . coerce
  nil = coerce HsNil

  (=%) x y = coerce $ HsOp "=%" (coerce x) (coerce y)

  and_ x y = coerce $ HsOp "∧" (coerce x) (coerce y)

  newtype Obs HsGen a = ObsHsGen {unObsHsGen :: String}
  observe = ObsHsGen . flip evalState 0 . ppHsQuote 1 . unReprHsGen

ppHsQuote :: Int -> HsQuote -> State Int String
ppHsQuote prec = \case
  HsLit l -> return l -- :: String -> HsQuote
  HsVar v -> return v -- :: String -> HsQuote
  HsForEach t b -> do
    -- :: HsQuote -> (HsQuote -> HsQuote) -> HsQuote
    t' <- ppHsQuote prec t
    let multilineT = not . null . tail $ lines t'
    x <- freshVar
    b' <- ppHsQuote prec $ b (HsVar x)
    let multilineB = not . null . tail $ lines b'
    return $
      "foreach "
        ++ ( if multilineT
               then "\n" ++ indent 2 t' ++ "  λ" --  ++ "\n "
               else t' ++ " λ"
           )
        ++ x
        ++ " ->"
        ++ (if multilineT then indent 2 else id)
          ( if multilineB
              then "\n" ++ indent 2 b' -- ++ "\n"
              else b'
          )
  HsWhere c b -> do
    -- :: HsQuote -> HsQuote -> HsQuote
    c' <- ppHsQuote prec c
    b' <- ppHsQuote prec b
    return $ "where_ " ++ c' ++ "\n" ++ indent 2 b'
  HsYield v -> do
    -- :: HsQuote -> HsQuote
    v' <- ppHsQuote prec v
    return $ "yield " ++ v'
  HsNil -> return "nil" -- :: HsQuote
  HsOp op x y -> do
    -- :: String -> HsQuote -> HsQuote -> HsQuote
    x' <- ppHsQuote prec x
    y' <- ppHsQuote prec y
    return $ x' ++ " " ++ op ++ " " ++ y'
  HsFnApp fn args -> do
    -- :: String -> [HsQuote] -> HsQuote
    args' <- traverse (ppHsQuote prec) args
    return $ fn ++ " " ++ unwords args'
  HsTable t -> return $ "table(" ++ t ++ ")"
  HsMkBag pairs -> do
    pairs' <-
      traverse
        ( \(k, v) -> do
            v' <- ppHsQuote prec v
            return $ k ++ " = " ++ v'
        )
        pairs

    return $ "⟨" ++ intercalate ", " pairs' ++ "⟩"
  HsProjBag b f -> do
    b' <- ppHsQuote prec b
    return $ b' ++ "." ++ f

freshVar :: State Int String
freshVar = do
  v <- get
  modify succ
  return ("x" ++ show v)

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

parenIndent :: Int -> Int -> String -> String
parenIndent outerPrec innerPrec str | outerPrec > innerPrec = "\n(\n" ++ indent 2 str ++ ")\n"
parenIndent _ _ str = str

paren :: Int -> Int -> String -> String
paren outerPrec innerPrec str | outerPrec > innerPrec = "(" ++ str ++ ")"
paren _ _ str = str
