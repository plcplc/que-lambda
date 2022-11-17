{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- See https://okmij.org/ftp/meta-programming/quel.pdf
module SqlDsl where

import Control.Monad.State
import Data.Kind
import Data.List

class Symantics r where
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

-- Meta-circular interpreter:

data R

instance Symantics R where
  newtype Repr R a = ReprIdentity {unReprIdentity :: a}

  int = ReprIdentity
  bool = ReprIdentity
  string = ReprIdentity

  foreach (ReprIdentity ls) f = ReprIdentity (concatMap (unReprIdentity . f . ReprIdentity) ls)
  where_ (ReprIdentity True) rows = rows
  where_ (ReprIdentity False) _ = ReprIdentity []

  yield (ReprIdentity x) = ReprIdentity [x]
  nil = ReprIdentity []

  (ReprIdentity x) =% (ReprIdentity y) = ReprIdentity (x == y)

  newtype Obs R a = ObsIdentity {unObsIdentity :: a}
  observe (ReprIdentity a) = ObsIdentity a

instance SymanticsOpen R where
  lam f = ReprIdentity (unReprIdentity . f . ReprIdentity)
  app (ReprIdentity f) (ReprIdentity x) = ReprIdentity (f x)

-- SQL interpreter (will likely need one per backend obviously)

data Sql

-- TODO: Model this more cleanly..
data SqlQuote
  = SqlSelect
      { sqlSelectFrom :: [SqlQuote],
        sqlSelectProj :: [SqlQuote],
        sqlSelectWhere :: SqlQuote
      }
  | SqlFromAlias SqlQuote String
  | SqlExpAnd SqlQuote SqlQuote
  | SqlExpEq SqlQuote SqlQuote
  | SqlExpInt Int
  | SqlExpBool Bool
  | SqlExpString String
  | SqlExpVar String
  | SqlProjTable String
  | SqlProjCol SqlQuote String
  deriving (Show)

ppSqlQuote :: SqlQuote -> String
ppSqlQuote = \case
  SqlSelect {..} ->
    "SELECT "
      ++ intercalate ", " (map ppSqlQuote sqlSelectProj)
      ++ " FROM ("
      ++ intercalate "), (" (map ppSqlQuote sqlSelectFrom)
      ++ ") WHERE "
      ++ ppSqlQuote sqlSelectWhere
  SqlFromAlias f a -> ppSqlQuote f ++ " AS " ++ a
  SqlExpAnd x1 x2 -> ppSqlQuote x1 ++ " AND " ++ ppSqlQuote x2
  SqlExpEq x1 x2 -> ppSqlQuote x1 ++ " = " ++ ppSqlQuote x2
  SqlExpInt i -> show i
  SqlExpBool b -> show b
  SqlExpString s -> show s
  SqlExpVar s -> s ++ ".*"
  SqlProjTable t -> t
  SqlProjCol (SqlExpVar t) c -> t ++ "." ++ c
  SqlProjCol tExp c -> "(" ++ ppSqlQuote tExp ++ ")." ++ c -- We should be able to model this case away

instance Symantics Sql where
  newtype Repr Sql a = ReprSql {unReprSql :: State Int SqlQuote}

  int = ReprSql . return . SqlExpInt
  bool = ReprSql . return . SqlExpBool
  string = ReprSql . return . SqlExpString

  foreach from body = ReprSql $ do
    f <- unReprSql from
    x <- freshVar
    b <- unReprSql (body (ReprSql (return (SqlExpVar x))))

    return $ case b of
      SqlSelect {sqlSelectFrom = f', sqlSelectProj = p', sqlSelectWhere = w'} ->
        SqlSelect
          { sqlSelectFrom = SqlFromAlias f x : f',
            sqlSelectProj = p',
            sqlSelectWhere = w'
          }
      err -> error $ "Incoherent argument to 'foreach: " ++ show err

  yield x = ReprSql $ do
    x' <- unReprSql x
    return
      SqlSelect
        { sqlSelectFrom = [],
          sqlSelectProj = [x'],
          sqlSelectWhere = SqlExpBool True
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
              sqlSelectWhere = w `SqlExpAnd` cond'
            }
      err -> error $ "Incoherent argument to 'where_': " ++ show err

  nil = ReprSql $ return $ SqlSelect [] [] (SqlExpBool False)

  (=%) x y = ReprSql $ do
    x' <- unReprSql x
    y' <- unReprSql y
    return $ SqlExpEq x' y'

  newtype Obs Sql a = ObsSql {unObsSql :: String}

  observe q =
    let q' = flip evalState 0 $ unReprSql q
     in ObsSql $ ppSqlQuote q'

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
  data Product r :: Type
  data Order r :: Type

  products :: Repr r [Product r]

  product_pid :: Repr r (Product r) -> Repr r Int
  product_name :: Repr r (Product r) -> Repr r String
  product_price :: Repr r (Product r) -> Repr r Int

  orders :: Repr r [Order r]

  order_oid :: Repr r (Order r) -> Repr r Int
  order_pid :: Repr r (Order r) -> Repr r Int
  order_qty :: Repr r (Order r) -> Repr r Int

-- * Meta-circular interpreter of the schema. Can only use a constant db. Interesting.

instance MyExampleSchema R where
  data Product R = ProductRow
    { productPid :: Int,
      productName :: String,
      productPrice :: Int
    }
    deriving (Show)
  data Order R = OrderRow
    { orderOid :: Int,
      orderPid :: Int,
      orderQty :: Int
    }
    deriving (Show)

  products =
    ReprIdentity
      [ ProductRow 1 "Tablet" 500,
        ProductRow 2 "Laptop" 1000,
        ProductRow 3 "Desktop" 1000,
        ProductRow 4 "Router" 150,
        ProductRow 5 "HDD" 100,
        ProductRow 6 "SSD" 500
      ]

  product_pid = ReprIdentity . productPid . unReprIdentity
  product_name = ReprIdentity . productName . unReprIdentity
  product_price = ReprIdentity . productPrice . unReprIdentity

  orders =
    ReprIdentity
      [ OrderRow 1 1 5,
        OrderRow 1 2 5,
        OrderRow 1 4 2,
        OrderRow 2 5 10,
        OrderRow 2 6 20,
        OrderRow 3 2 50
      ]

  order_oid = ReprIdentity . orderOid . unReprIdentity
  order_pid = ReprIdentity . orderPid . unReprIdentity
  order_qty = ReprIdentity . orderQty . unReprIdentity

-- * Sql interpreter for MyExampleSchema

instance MyExampleSchema Sql where
  data Product Sql = Product
  data Order Sql = Order

  products = table "products"

  product_pid = projCol "pid"
  product_name = projCol "name"
  product_price = projCol "price"

  orders = table "orders"

  order_oid = projCol "oid"
  order_pid = projCol "pid"
  order_qty = projCol "qty"

table :: String -> Repr Sql a
table tableName =
  ReprSql $ do
    x <- freshVar
    return $
      SqlSelect
        { sqlSelectFrom = [SqlFromAlias (SqlProjTable tableName) x ],
          sqlSelectProj = [SqlExpVar x],
          sqlSelectWhere = SqlExpBool True
        }

projCol :: String -> Repr Sql a -> Repr Sql b
projCol col tableAlias = ReprSql $ do
  tableAlias' <- unReprSql tableAlias
  return $ SqlProjCol tableAlias' col

q1 :: (MyExampleSchema r, Symantics r) => Int -> Repr r [Order r]
q1 oid' =
  foreach orders \order ->
    where_ (int oid' =% order_oid order) (yield order)

test :: [Order R]
test = unReprIdentity (q1 1)
