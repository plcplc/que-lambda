# Relational Query DSL Experiments

(derived from https://okmij.org/ftp/meta-programming/quel.pdf)

## Example Queries

```haskell
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
```

## Example Translations

```
cabal repl que-lambda:lib:que-lambda
ghci>
ghci> :l QueryExamples
ghci>
ghci> putStrLn $ unObsHsGen  $ observe $ q3 1
foreach
  foreach table(orders) λx0 ->
    where_ 1 =% x0.oid
      yield x0
  λx1 ->
    foreach table(products) λx2 ->
      where_ x2.pid =% x1.pid
        yield ⟨pid = x2.pid, name = x2.name, sale = x2.price * x1.qty⟩

ghci>
ghci>
ghci> putStrLn $ unObsHsGen . unObsForFor  $ observe $ q3 1
foreach table(orders) λx0 ->
  foreach
    where_ 1 =% x0.oid
      yield x0
    λx1 ->
      foreach table(products) λx2 ->
        where_ x2.pid =% x1.pid
          yield ⟨pid = x2.pid, name = x2.name, sale = x2.price * x1.qty⟩

ghci>
ghci>
ghci> putStrLn $ unObsHsGen . unObsForWhere1 . unObsForFor  $ observe $ q3 1
foreach table(orders) λx0 ->
  where_ 1 =% x0.oid
    foreach yield x0 λx1 ->
      foreach table(products) λx2 ->
        where_ x2.pid =% x1.pid
          yield ⟨pid = x2.pid, name = x2.name, sale = x2.price * x1.qty⟩

ghci>
ghci>
ghci> putStrLn $ unObsHsGen . unObsForYield . unObsForWhere1 . unObsForFor $ observe $ q3 1
foreach table(orders) λx0 ->
  where_ 1 =% x0.oid
    foreach table(products) λx1 ->
      where_ x1.pid =% x0.pid
        yield ⟨pid = x1.pid, name = x1.name, sale = x1.price * x0.qty⟩

ghci>
ghci>
ghci> putStrLn $ unObsSql . unObsForYield . unObsForWhere1 . unObsForFor $ observe $ q3 1
SELECT x1.pid AS pid, x1.name AS name, x1.price * x0.qty AS sale FROM orders AS x0, products AS x1 WHERE 1 = x0.oid AND x1.pid = x0.pid
ghci>
ghc

```
