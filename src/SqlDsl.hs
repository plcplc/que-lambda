{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- See https://okmij.org/ftp/meta-programming/quel.pdf
module SqlDsl where

import Control.Applicative
import Control.Monad.State
import Data.Kind
import Data.List

class (Num (Repr r Int)) => Symantics r where
  data Repr r :: Type -> Type

  -- TODO: Delegate to separate type class.
  int :: Int -> Repr r Int
  bool :: Bool -> Repr r Bool
  string :: String -> Repr r String

  foreach :: Repr r [a] -> (Repr r a -> Repr r [b]) -> Repr r [b]
  where_ :: Repr r Bool -> Repr r [a] -> Repr r [a]
  yield :: Repr r a -> Repr r [a]
  nil :: Repr r [a]

  (=%) :: (Eq a) => Repr r a -> Repr r a -> Repr r Bool
  -- ^
  -- Q: 'Eq a' required for meta-circular interpreter.
  --    How can we avoid that?

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

-- * Optimization passes

-- Rule ForFor

data ForFor r

instance (Symantics r) => Num (Repr (ForFor r) Int) where
  (+) x y = Unknown ((+) (dyn x) (dyn y))
  (*) x y = Unknown ((*) (dyn x) (dyn y))
  abs = Unknown . abs . dyn
  signum = Unknown . signum . dyn
  fromInteger = dyn . fromInteger
  negate = Unknown . negate . dyn

instance Symantics r => Symantics (ForFor r) where
  data Repr (ForFor r) a where
    ForEach :: Repr (ForFor r) [a] -> (Repr (ForFor r) a -> Repr (ForFor r) [b]) -> Repr (ForFor r) [b]
    Unknown :: Repr r a -> Repr (ForFor r) a

  int = Unknown . int
  bool = Unknown . bool
  string = Unknown . string

  {-

     foreach (foreach L (\y -> M)) (\x -> N)

   ~~~>

     foreach L (\y -> foreach M (\x -> N))

     -}
  foreach s _N = case s of
    Unknown {} -> ForEach s _N
    ForEach _L _M ->
      Unknown $ foreach (dyn _L) (\y -> foreach (dyn $ _M $ Unknown y) (dyn . _N . Unknown))

  where_ cond body = Unknown $ where_ (dyn cond) (dyn body)
  yield y = Unknown $ yield (dyn y)
  nil = Unknown nil

  (=%) a b = Unknown $ (=%) (dyn a) (dyn b)

  newtype Obs (ForFor r) a = ObsForFor {unObsForFor :: Obs r a}
  observe = ObsForFor . observe . dyn

dyn :: Symantics r => Repr (ForFor r) a -> Repr r a
dyn (Unknown u) = u
dyn (ForEach f b) = foreach (dyn f) (dyn . b . Unknown)

-- ForWhere1 is the next thing needed to normalize q3, I think.

-- Meta-circular interpreter:

data R

-- Somehow this cannot be derived??
instance Applicative (Repr R) where
  pure = ReprIdentity

  f <*> x = ReprIdentity (unReprIdentity f (unReprIdentity x))

instance Num (Repr R Int) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = ReprIdentity . fromInteger
  negate = fmap negate

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

  newtype Obs R a = ObsIdentity {unObsIdentity :: a}
  observe (ReprIdentity a) = ObsIdentity a

instance SymanticsOpen R where
  lam f = ReprIdentity (unReprIdentity . f . ReprIdentity)
  app = (<*>)

-- SQL interpreter (will likely need one per backend obviously)

data Sql

-- TODO: Model this more cleanly..
data SqlQuote
  = SqlSelect
      { sqlSelectFrom :: [SqlQuote],
        sqlSelectProj :: [SqlQuote],
        sqlSelectWhere :: [SqlQuote]
      }
  | -- | SqlTable String [String]
    SqlFromAlias SqlQuote String
  | SqlExpAnd SqlQuote SqlQuote
  | SqlExpEq SqlQuote SqlQuote
  | SqlExpInt Int
  | SqlExpBool Bool
  | SqlExpOp String SqlQuote SqlQuote
  | SqlExpFunApp String [SqlQuote]
  | SqlExpString String
  | SqlExpVar String
  | SqlMakeLabels [(String, SqlQuote)] -- ⟨l=B⟩
  | SqlProjTable String
  | SqlProjCol SqlQuote String
  | SqlProjAlias SqlQuote String
  deriving (Show)

ppSqlQuote :: Int -> SqlQuote -> String
ppSqlQuote prec = \case
  SqlSelect {..} ->
    parenIndent prec 0 $
      "SELECT "
        ++ intercalate ", " (map (ppSqlQuote 1) sqlSelectProj)
        ++ " "
        ++ concat
          [ "FROM "
              ++ intercalate ", " (map (ppSqlQuote 1) sqlSelectFrom)
            | not $ null sqlSelectFrom
          ]
        ++ " "
        ++ concat
          [ "WHERE "
              ++ ppSqlQuote 1 (foldr1 SqlExpAnd sqlSelectWhere)
            | not $ null sqlSelectWhere
          ]
  -- SqlTable table _cols -> parenIndent prec 0 $ "SELECT * FROM " ++ table
  SqlFromAlias f a -> ppSqlQuote prec f ++ " AS " ++ a
  SqlExpAnd x1 x2 -> ppSqlQuote prec x1 ++ " AND " ++ ppSqlQuote prec x2
  SqlExpEq x1 x2 -> ppSqlQuote prec x1 ++ " = " ++ ppSqlQuote prec x2
  SqlExpInt i -> show i
  SqlExpBool b -> show b
  SqlExpOp op x y -> ppSqlQuote prec x ++ " " ++ op ++ " " ++ ppSqlQuote prec y
  SqlExpFunApp f args -> f ++ "(" ++ intercalate ", " (map (ppSqlQuote 0) args) ++ ")"
  SqlExpString s -> show s
  SqlExpVar s -> s ++ ".*"
  SqlMakeLabels labels -> intercalate ", " (map (\(l, exp') -> ppSqlQuote prec exp' ++ " AS " ++ l) labels)
  SqlProjAlias t alias -> ppSqlQuote prec t ++ " AS " ++ alias
  SqlProjTable t -> t
  SqlProjCol (SqlExpVar t) c -> t ++ "." ++ c
  SqlProjCol tExp c -> ppSqlQuote 1 tExp ++ c -- We should be able to model this case away

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

parenIndent :: Int -> Int -> String -> String
parenIndent outerPrec innerPrec str | outerPrec > innerPrec = "\n(\n" ++ indent 2 str ++ ")\n"
parenIndent _ _ str = str

paren :: Int -> Int -> String -> String
paren outerPrec innerPrec str | outerPrec > innerPrec = "(" ++ str ++ ")"
paren _ _ str = str

instance Num (Repr Sql Int) where
  (+) x y = ReprSql $ do
    x' <- unReprSql x
    y' <- unReprSql y
    return $ SqlExpOp "+" x' y'
  (-) x y = ReprSql $ do
    x' <- unReprSql x
    y' <- unReprSql y
    return $ SqlExpOp "-" x' y'
  (*) x y = ReprSql $ do
    x' <- unReprSql x
    y' <- unReprSql y
    return $ SqlExpOp "*" x' y'
  abs x = ReprSql $ do
    x' <- unReprSql x
    return $ SqlExpFunApp "abs" [x']
  signum x = ReprSql $ do
    x' <- unReprSql x
    return $ SqlExpFunApp "signum" [x']
  fromInteger = int . fromInteger

instance Symantics Sql where
  newtype Repr Sql a = ReprSql {unReprSql :: State Int SqlQuote}

  int = ReprSql . return . SqlExpInt
  bool = ReprSql . return . SqlExpBool
  string = ReprSql . return . SqlExpString

  foreach from body = ReprSql $ do
    f <- unReprSql from
    x <- freshVar
    b <- unReprSql (body (ReprSql (return (SqlExpVar x))))

    f' <- case f of
      -- This is a bit awkward. In terms of th paper, 
      -- we cannot match on "for (x <- table(..) F'",
      -- because all we have is "for S[[B]] S[[F']]".
      -- We would have been able to, if we had made "Repr Sql a" an initial
      -- encoding of the language and then done the translation into SqlQuote
      -- in "observe". This would also have made "SqlQuote" cleaner.
      SqlSelect {sqlSelectFrom = [SqlProjTable t]} ->
        return (SqlProjTable t)
      --unexpected -> return unexpected
      unexpected -> error $ "Non-normalized argument to 'foreach' table: " ++ show unexpected
    return $ case b of
      SqlSelect {sqlSelectFrom = f'', sqlSelectProj = p', sqlSelectWhere = w'} ->
        SqlSelect
          { sqlSelectFrom = SqlFromAlias f' x : f'',
            sqlSelectProj = p',
            sqlSelectWhere = w'
          }
      err -> error $ "Non-normalized argument to 'foreach' body: " ++ show err

  yield x = ReprSql $ do
    x' <- unReprSql x
    return
      SqlSelect
        { sqlSelectFrom = [],
          sqlSelectProj = [x'],
          sqlSelectWhere = []
        }

  where_ cond body = ReprSql $ do
    cond' <- unReprSql cond
    body' <- unReprSql body
    case body' of
      SqlSelect {sqlSelectFrom = f, sqlSelectProj = p, sqlSelectWhere = w} ->
        return
          SqlSelect
            { sqlSelectFrom = f,
              sqlSelectProj = p,
              sqlSelectWhere = cond' : w
            }
      err -> error $ "Incoherent argument to 'where_': " ++ show err

  nil = ReprSql $ return $ SqlSelect [] [] [SqlExpBool False]

  (=%) x y = ReprSql $ do
    x' <- unReprSql x
    y' <- unReprSql y
    return $ SqlExpEq x' y'

  newtype Obs Sql a = ObsSql {unObsSql :: String}

  observe q =
    let q' = flip evalState 0 $ unReprSql q
     in ObsSql $ ppSqlQuote 0 q'

freshVar :: State Int String
freshVar = do
  v <- get
  modify succ
  return ("x" ++ show v)

-- * Schema mapping

-- The paper takes some liberties wrt to how to map the data source schemas
-- into the DSL:
--

-- * In the logic-sections they conveniently assume a 'table(t)' function exists.

-- * In the actual implementation appendix they final-encode the schema and require all interpreters be extended accordingly.

--
-- This seems like an excessive requirement for an implementation, which we'll have to overcome.
--
-- Probably this is not insurmountable for HGE, since we will just have a
-- single generic way to postulate existence of tables an columns by fiat of
-- metadata, which is going to be dynamic anyway.
--
-- But it seems less appetizing for an app that encodes its schema statically.
-- E.g., why does declaring I have a table 'foo' require me to extend the
-- pretty printer and a meta-circular interpreter?

-- Examples:

--  Database:
--  +----------------------+  +-----------------+
--  |Products              |  | Orders          |
--  +----------------------+  +-----------------+
--  | pid | name    | price|  | oid | pid | qty |
--  |----------------------|  |-----|-----| ----|
--  | 1   | Tablet  | 500  |  | 1   | 1   | 5   |
--  | 2   | Laptop  | 1,000|  | 1   | 2   | 5   |
--  | 3   | Desktop | 1,000|  | 1   | 4   | 2   |
--  | 4   | Router  | 150  |  | 2   | 5   | 10  |
--  | 5   | HDD     | 100  |  | 2   | 6   | 20  |
--  | 6   | SSD     | 500  |  | 3   | 2   | 50  |
--  +----------------------+  +-----------------+

class Symantics r => MyExampleSchema r where
  type Product r :: Type

  products :: Repr r [Product r]

  product_pid :: Repr r (Product r) -> Repr r Int
  product_name :: Repr r (Product r) -> Repr r String
  product_price :: Repr r (Product r) -> Repr r Int

  type Order r :: Type

  orders :: Repr r [Order r]

  order_oid :: Repr r (Order r) -> Repr r Int
  order_pid :: Repr r (Order r) -> Repr r Int
  order_qty :: Repr r (Order r) -> Repr r Int

  type Sales r :: Type

  mkSales :: Repr r Int -> Repr r String -> Repr r Int -> Repr r (Sales r)

-- * Meta-circular interpreter of the schema. Can only use a constant db. Interesting.

--
data ProductR = ProductR
  { productPid :: Int,
    productName :: String,
    productPrice :: Int
  }
  deriving (Show)

data OrderR = OrderR
  { orderOid :: Int,
    orderPid :: Int,
    orderQty :: Int
  }
  deriving (Show)

data SalesR = SalesR
  { salesPid :: Int,
    salesName :: String,
    salesSale :: Int
  }
  deriving (Show)

instance MyExampleSchema R where
  type Product R = ProductR

  products =
    ReprIdentity
      [ ProductR 1 "Tablet" 500,
        ProductR 2 "Laptop" 1000,
        ProductR 3 "Desktop" 1000,
        ProductR 4 "Router" 150,
        ProductR 5 "HDD" 100,
        ProductR 6 "SSD" 500
      ]

  product_pid = ReprIdentity . productPid . unReprIdentity
  product_name = ReprIdentity . productName . unReprIdentity
  product_price = ReprIdentity . productPrice . unReprIdentity

  type Order R = OrderR

  orders =
    ReprIdentity
      [ OrderR 1 1 5,
        OrderR 1 2 5,
        OrderR 1 4 2,
        OrderR 2 5 10,
        OrderR 2 6 20,
        OrderR 3 2 50
      ]

  order_oid = ReprIdentity . orderOid . unReprIdentity
  order_pid = ReprIdentity . orderPid . unReprIdentity
  order_qty = ReprIdentity . orderQty . unReprIdentity

  type Sales R = SalesR

  mkSales pid name sale = ReprIdentity $ SalesR (unReprIdentity pid) (unReprIdentity name) (unReprIdentity sale)

-- * ForFor interpreter for MyExampleSchema

instance (MyExampleSchema r) => MyExampleSchema (ForFor r) where
  type Product (ForFor r) = Product r
  products = Unknown products

  product_pid = Unknown . product_pid . dyn
  product_name = Unknown . product_name . dyn
  product_price = Unknown . product_price . dyn

  type Order (ForFor r) = Order r

  orders = Unknown orders

  order_oid = Unknown . order_oid . dyn
  order_pid = Unknown . order_pid . dyn
  order_qty = Unknown . order_qty . dyn

  type Sales (ForFor r) = Sales r

  mkSales pid name sale = Unknown $ mkSales (dyn pid) (dyn name) (dyn sale)

-- * Sql interpreter for MyExampleSchema

instance MyExampleSchema Sql where
  type Product Sql = ()

  products = declareTable "products" ["pid", "name", "price"]

  product_pid = projCol "pid"
  product_name = projCol "name"
  product_price = projCol "price"

  type Order Sql = ()

  orders = declareTable "orders" ["oid", "pid", "qty"]

  order_oid = projCol "oid"
  order_pid = projCol "pid"
  order_qty = projCol "qty"

  type Sales Sql = ()

  mkSales pid name sale = ReprSql $ do
    pid' <- unReprSql pid
    name' <- unReprSql name
    sale' <- unReprSql sale
    return $
      -- TODO: Actually model ⟨l=B, ..⟩ in AST!
      SqlMakeLabels [("pid", pid'), ("name", name'), ("sale", sale')]

{-
 SqlSelect
   { sqlSelectFrom = [],
     sqlSelectProj =
       [ SqlProjAlias pid' "pid",
         SqlProjAlias name' "name",
         SqlProjAlias sale' "sale"
       ],
     sqlSelectWhere = []
   }
   -}

declareTable :: String -> [String] -> Repr Sql a
-- declareTable tableName cols = ReprSql $ return $ SqlTable tableName cols
declareTable tableName cols =
  ReprSql $
    return $
      SqlSelect
        { sqlSelectFrom = [SqlProjTable tableName],
          sqlSelectProj = [SqlProjCol (SqlExpVar tableName) col | col <- cols],
          sqlSelectWhere = []
        }

projCol :: String -> Repr Sql a -> Repr Sql b
projCol col tableAlias = ReprSql $ do
  tableAlias' <- unReprSql tableAlias
  return $ SqlProjCol tableAlias' col

q1 :: (MyExampleSchema r, Symantics r) => Int -> Repr r [Order r]
q1 oid' =
  foreach orders \order ->
    where_ (int oid' =% order_oid order) (yield order)

q2 :: (MyExampleSchema r, Symantics r) => Repr r (Order r) -> Repr r [Sales r]
q2 o = foreach products \p ->
  where_
    (product_pid p =% order_pid o)
    ( yield
        ( mkSales
            (product_pid p)
            (product_name p)
            (product_price p * order_qty o)
        )
    )

q3 :: (MyExampleSchema r, Symantics r) => Int -> Repr r [Sales r]
q3 oid' = foreach (q1 oid') q2

test :: [Sales R]
test = unReprIdentity (q3 1)
