module QueryExamples where

import Data.Coerce
import Data.Kind
import QueLambda.GenSql
import QueLambda.Hs
import QueLambda.MetaCircular
import QueLambda.Optimizations.ForFor as ForFor
import QueLambda.Optimizations.ForWhere1 as ForWhere1
import QueLambda.Optimizations.ForYield as ForYield
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
  products = liftRepr products

  product_pid = liftRepr . product_pid . unliftRepr
  product_name = liftRepr . product_name . unliftRepr
  product_price = liftRepr . product_price . unliftRepr

  type Order (ForFor r) = Order r

  orders = liftRepr orders

  order_oid = liftRepr . order_oid . unliftRepr
  order_pid = liftRepr . order_pid . unliftRepr
  order_qty = liftRepr . order_qty . unliftRepr

  type Sales (ForFor r) = Sales r

  mkSales pid name sale = liftRepr $ mkSales (unliftRepr pid) (unliftRepr name) (unliftRepr sale)

-- * ForWhere1 interpreter for MyExampleSchema

instance (MyExampleSchema r) => MyExampleSchema (ForWhere1 r) where
  type Product (ForWhere1 r) = Product r
  products = liftRepr products

  product_pid = liftRepr . product_pid . unliftRepr
  product_name = liftRepr . product_name . unliftRepr
  product_price = liftRepr . product_price . unliftRepr

  type Order (ForWhere1 r) = Order r

  orders = liftRepr orders

  order_oid = liftRepr . order_oid . unliftRepr
  order_pid = liftRepr . order_pid . unliftRepr
  order_qty = liftRepr . order_qty . unliftRepr

  type Sales (ForWhere1 r) = Sales r

  mkSales pid name sale = liftRepr $ mkSales (unliftRepr pid) (unliftRepr name) (unliftRepr sale)

-- * ForYield interpreter for MyExampleSchema

instance (MyExampleSchema r) => MyExampleSchema (ForYield r) where
  type Product (ForYield r) = Product r
  products = liftRepr products

  product_pid = liftRepr . product_pid . unliftRepr
  product_name = liftRepr . product_name . unliftRepr
  product_price = liftRepr . product_price . unliftRepr

  type Order (ForYield r) = Order r

  orders = liftRepr orders

  order_oid = liftRepr . order_oid . unliftRepr
  order_pid = liftRepr . order_pid . unliftRepr
  order_qty = liftRepr . order_qty . unliftRepr

  type Sales (ForYield r) = Sales r

  mkSales pid name sale = liftRepr $ mkSales (unliftRepr pid) (unliftRepr name) (unliftRepr sale)

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

-- * HsGen interpreter for MyExampleSchema

instance MyExampleSchema HsGen where
  type Product HsGen = ()

  products = ReprHsGen $ HsTable "products"

  product_pid b = ReprHsGen $ HsProjBag (coerce b) "pid"
  product_name b = ReprHsGen $ HsProjBag (coerce b) "name"
  product_price b = ReprHsGen $ HsProjBag (coerce b) "price"

  type Order HsGen = ()

  orders = ReprHsGen $ HsTable "orders"

  order_oid b = ReprHsGen $ HsProjBag (coerce b) "oid"
  order_pid b = ReprHsGen $ HsProjBag (coerce b) "pid"
  order_qty b = ReprHsGen $ HsProjBag (coerce b) "qty"

  type Sales HsGen = ()

  mkSales pid name sale =
    ReprHsGen $
      HsMkBag
        [ ("pid", coerce pid),
          ("name", coerce name),
          ("sale", coerce sale)
        ]

-- Queries

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
