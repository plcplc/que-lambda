module QueryExamples where

import Control.Monad.Except
import Data.Coerce
import QueLambda.GenSql
import QueLambda.Hs
import QueLambda.MetaCircular
import QueLambda.Optimizations.ForFor as ForFor
import QueLambda.Optimizations.ForWhere1 as ForWhere1
import QueLambda.Optimizations.ForYield as ForYield
import QueLambda.Optimizations.WhereFor as WhereFor
import QueLambda.Optimizations.WhereWhere as WhereWhere
import QueLambda.Symantics

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

data Product = Product
  { productPid :: Int,
    productName :: String,
    productPrice :: Int
  }
  deriving (Show)

data Order = Order
  { orderOid :: Int,
    orderPid :: Int,
    orderQty :: Int
  }
  deriving (Show)

data Sales = Sales
  { salesPid :: Int,
    salesName :: String,
    salesSale :: Int
  }
  deriving (Show)

type SchemaIndifferent r s =
  ( UnliftRepr r s,
    LiftRepr r s,
    MyExampleSchema s
  )

class Symantics r => MyExampleSchema r where
  products :: Repr r [Product]
  default products :: SchemaIndifferent r s => Repr r [Product]
  products = liftRepr products

  product_pid :: Repr r Product -> Repr r Int
  default product_pid :: SchemaIndifferent r s => Repr r Product -> Repr r Int
  product_pid = liftRepr . product_pid . unliftRepr

  product_name :: Repr r Product -> Repr r String
  default product_name :: SchemaIndifferent r s => Repr r Product -> Repr r String
  product_name = liftRepr . product_name . unliftRepr

  product_price :: Repr r Product -> Repr r Int
  default product_price :: SchemaIndifferent r s => Repr r Product -> Repr r Int
  product_price = liftRepr . product_price . unliftRepr

  orders :: Repr r [Order]
  default orders :: SchemaIndifferent r s => Repr r [Order]
  orders = liftRepr orders

  order_oid :: Repr r Order -> Repr r Int
  default order_oid :: SchemaIndifferent r s => Repr r Order -> Repr r Int
  order_oid = liftRepr . order_oid . unliftRepr

  order_pid :: Repr r Order -> Repr r Int
  default order_pid :: SchemaIndifferent r s => Repr r Order -> Repr r Int
  order_pid = liftRepr . order_pid . unliftRepr

  order_qty :: Repr r Order -> Repr r Int
  default order_qty :: SchemaIndifferent r s => Repr r Order -> Repr r Int
  order_qty = liftRepr . order_qty . unliftRepr

  mkSales :: Repr r Int -> Repr r String -> Repr r Int -> Repr r Sales
  default mkSales :: SchemaIndifferent r s => Repr r Int -> Repr r String -> Repr r Int -> Repr r Sales
  mkSales pid name sale = liftRepr $ mkSales (unliftRepr pid) (unliftRepr name) (unliftRepr sale)

-- * Meta-circular interpreter of the schema. Can only use a constant db. Interesting.

instance MyExampleSchema R where
  products =
    ReprIdentity
      [ Product 1 "Tablet" 500,
        Product 2 "Laptop" 1000,
        Product 3 "Desktop" 1000,
        Product 4 "Router" 150,
        Product 5 "HDD" 100,
        Product 6 "SSD" 500
      ]

  product_pid = ReprIdentity . productPid . unReprIdentity
  product_name = ReprIdentity . productName . unReprIdentity
  product_price = ReprIdentity . productPrice . unReprIdentity

  orders =
    ReprIdentity
      [ Order 1 1 5,
        Order 1 2 5,
        Order 1 4 2,
        Order 2 5 10,
        Order 2 6 20,
        Order 3 2 50
      ]

  order_oid = ReprIdentity . orderOid . unReprIdentity
  order_pid = ReprIdentity . orderPid . unReprIdentity
  order_qty = ReprIdentity . orderQty . unReprIdentity

  mkSales pid name sale = ReprIdentity $ Sales (unReprIdentity pid) (unReprIdentity name) (unReprIdentity sale)

-- Optimization pass instances for MyExampleSchema

-- (These are all SchemaIndifferent, so the default implementations will do)

instance MyExampleSchema r => MyExampleSchema (ForFor r)

instance MyExampleSchema r => MyExampleSchema (ForWhere1 r)

instance MyExampleSchema r => MyExampleSchema (ForYield r)

instance MyExampleSchema r => MyExampleSchema (WhereFor r)

instance MyExampleSchema r => MyExampleSchema (WhereWhere r)

-- * Sql interpreter for MyExampleSchema

instance MyExampleSchema Sql where
  products = declareTable "products"

  product_pid = projCol "pid"
  product_name = projCol "name"
  product_price = projCol "price"

  orders = declareTable "orders"

  order_oid = projCol "oid"
  order_pid = projCol "pid"
  order_qty = projCol "qty"

  mkSales pid name sale = ReprSql $ do
    pid' <- refinePrimitive pid
    name' <- refinePrimitive name
    sale' <- refinePrimitive sale
    return $
      Record $
        NqlRecordCon [("pid", pid'), ("name", name'), ("sale", sale')]

declareTable :: String -> Repr Sql a
declareTable tableName = ReprSql $ return $ Body $ NqlTable tableName

projCol :: String -> Repr Sql a -> Repr Sql b
projCol col tableExp = ReprSql $ do
  tableAlias' <- unReprSql tableExp
  tableAlias <- case tableAlias' of
    Record (NqlVar tableAlias) -> return tableAlias
    _ -> throwError "Unnormalized argument to : `projCol col tableExp`: `tableExp'` was not a variable!"
  return $ Primitives $ NqlRecordProj tableAlias col

-- * HsGen interpreter for MyExampleSchema

instance MyExampleSchema HsGen where
  products = ReprHsGen $ HsTable "products"

  product_pid b = ReprHsGen $ HsProjBag (coerce b) "pid"
  product_name b = ReprHsGen $ HsProjBag (coerce b) "name"
  product_price b = ReprHsGen $ HsProjBag (coerce b) "price"

  orders = ReprHsGen $ HsTable "orders"

  order_oid b = ReprHsGen $ HsProjBag (coerce b) "oid"
  order_pid b = ReprHsGen $ HsProjBag (coerce b) "pid"
  order_qty b = ReprHsGen $ HsProjBag (coerce b) "qty"

  mkSales pid name sale =
    ReprHsGen $
      HsMkBag
        [ ("pid", coerce pid),
          ("name", coerce name),
          ("sale", coerce sale)
        ]

-- Queries

q1 :: (MyExampleSchema r, Symantics r) => Int -> Repr r [Order]
q1 oid' =
  foreach orders \order ->
    where_ (int oid' =% order_oid order) (yield order)

q2 :: (MyExampleSchema r, Symantics r) => Repr r Order -> Repr r [Sales]
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

q3 :: (MyExampleSchema r, Symantics r) => Int -> Repr r [Sales]
q3 oid' = foreach (q1 oid') q2

test :: [Sales]
test = unReprIdentity (q3 1)
