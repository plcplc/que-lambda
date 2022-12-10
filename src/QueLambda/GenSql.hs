-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.GenSql where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Char
import Data.List
import QueLambda.Symantics

data Sql

data SqlStatementsQuote
  = SqlUnionAll SqlStatementsQuote SqlStatementsQuote
  | SqlStatementsSingle SqlStatementQuote
  deriving (Show)

data SqlStatementQuote = SqlSelect
  { sqlSelectFrom :: [SqlFromQuote],
    sqlSelectProj :: SqlProjectionsQuote,
    sqlSelectWhere :: [SqlExpressionQuote]
  }
  deriving (Show)

data SqlFromQuote
  = SqlFromTable String
  | SqlFromTableAliased String String
  deriving (Show)

data SqlProjectionsQuote
  = SqlProjectionsAliased [(SqlExpressionQuote, String)]
  | SqlProjectionNull
  | SqlProjectionStar String
  deriving (Show)

-- \| SqlFromStatement SqlStatementQuote -- We shouldn't be producing this with normalized queries.

data SqlExpressionQuote
  = SqlExpAnd SqlExpressionQuote SqlExpressionQuote
  | SqlExpBool Bool
  | SqlExpFunApp String [SqlExpressionQuote]
  | SqlExpInt Int
  | SqlExpNull
  | SqlExpOpApp String SqlExpressionQuote SqlExpressionQuote
  | SqlExpProj String String
  | SqlExpString String
  | SqlExpVar String
  deriving (Show)

ppSql :: SqlStatementsQuote -> String
ppSql = ppSqlStatementsQuote 0

ppSqlStatementsQuote :: Int -> SqlStatementsQuote -> String
ppSqlStatementsQuote prec = \case
  SqlUnionAll s1 s2 -> parenIndent prec 1 (ppSqlStatementsQuote 1 s1) ++ "UNION ALL" ++ parenIndent prec 1 (ppSqlStatementsQuote 1 s2)
  SqlStatementsSingle statement -> ppSqlStatementQuote prec statement

ppSqlStatementQuote :: Int -> SqlStatementQuote -> String
ppSqlStatementQuote
  prec
  SqlSelect
    { sqlSelectFrom = froms,
      sqlSelectProj = projs,
      sqlSelectWhere = conds
    } =
    parenIndent
      prec
      0
      ( "SELECT\n"
          ++ indent 2 (ppSqlProjectionsQuote projs)
          ++ "FROM\n"
          ++ indent 2 (intercalate ",\n" (map ppSqlFromQuote froms))
          ++ "WHERE\n"
          ++ indent 2 (intercalate " AND " (map (ppSqlExpressionQuote 0) conds))
      )

ppSqlExpressionQuote :: Int -> SqlExpressionQuote -> String
ppSqlExpressionQuote prec = \case
  SqlExpAnd e1 e2 -> parenIndent prec 0 (ppSqlExpressionQuote 0 e1 ++ " AND " ++ ppSqlExpressionQuote 0 e2)
  SqlExpBool b -> show b
  SqlExpFunApp fn args -> fn ++ "(" ++ intercalate ", " (map (ppSqlExpressionQuote 1) args) ++ ")"
  SqlExpInt i -> show i
  SqlExpNull -> "NULL"
  SqlExpOpApp "+" x y -> paren prec 0 (ppSqlExpressionQuote 0 x ++ " + " ++ ppSqlExpressionQuote 0 y)
  SqlExpOpApp "*" x y -> paren prec 1 (ppSqlExpressionQuote 1 x ++ " * " ++ ppSqlExpressionQuote 1 y)
  SqlExpOpApp op x y -> ppSqlExpressionQuote 1 x ++ " " ++ op ++ " " ++ ppSqlExpressionQuote 1 y
  SqlExpProj table col -> table ++ "." ++ col
  SqlExpString str -> show str
  SqlExpVar var -> var

ppSqlFromQuote :: SqlFromQuote -> String
ppSqlFromQuote = \case
  SqlFromTable table -> table
  SqlFromTableAliased table alias -> table ++ " AS " ++ alias

ppSqlProjectionsQuote :: SqlProjectionsQuote -> String
ppSqlProjectionsQuote = \case
  SqlProjectionsAliased aliased -> intercalate ",\n" (map (\(ex, alias) -> ppSqlExpressionQuote 1 ex ++ " AS " ++ alias) aliased)
  SqlProjectionNull -> "NULL"
  SqlProjectionStar table -> table ++ ".*"

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

parenIndent :: Int -> Int -> String -> String
parenIndent outerPrec innerPrec str | outerPrec > innerPrec = "\n(\n" ++ indent 2 str ++ ")\n"
parenIndent _ _ str = str

paren :: Int -> Int -> String -> String
paren outerPrec innerPrec str | outerPrec > innerPrec = "(" ++ str ++ ")"
paren _ _ str = str

refinePrimitive :: Repr Sql a -> ReaderT Int (Except String) NqlPrimitives
refinePrimitive (ReprSql x') = do
  x <- x'
  case x of
    Primitives p -> return p
    _ -> throwError "Not a primitive"

instance Num (Repr Sql Int) where
  (+) x y = ReprSql $ do
    x' <- refinePrimitive x
    y' <- refinePrimitive y
    return $ Primitives $ NqlFunctionApp "+" [x', y']
  (-) x y = ReprSql $ do
    x' <- refinePrimitive x
    y' <- refinePrimitive y
    return $ Primitives $ NqlFunctionApp "-" [x', y']
  (*) x y = ReprSql $ do
    x' <- refinePrimitive x
    y' <- refinePrimitive y
    return $ Primitives $ NqlFunctionApp "*" [x', y']
  abs x = ReprSql $ do
    x' <- refinePrimitive x
    return $ Primitives $ NqlFunctionApp "abs" [x']
  signum x = ReprSql $ do
    x' <- refinePrimitive x
    return $ Primitives $ NqlFunctionApp "signum" [x']
  fromInteger = int . fromInteger

data NormalizedQueLam
  = Queries NqlQueries
  | Comprehensions NqlComprehensions
  | Body NqlBody
  | Record NqlRecord
  | Primitives NqlPrimitives

data NqlQueries
  = NqlUnion NqlQueries NqlQueries
  | NqlNil
  | NqlComprehensionQuerySub NqlComprehensions

data NqlComprehensions
  = NqlFor String {- table -} (String -> NqlComprehensions)
  | NqlBodyComprehensionSub NqlBody

data NqlBody
  = NqlWhere NqlPrimitives NqlBody
  | NqlYield NqlRecord
  | NqlTable String

data NqlRecord
  = NqlRecordCon [(String, NqlPrimitives)]
  | NqlVar String

data NqlPrimitives
  = NqlRecordProj String String
  | NqlFunctionApp String [NqlPrimitives]
  | NqlInt Int
  | NqlBool Bool
  | NqlString String

sqlQuote :: forall m. Monad m => NqlQueries -> ReaderT Int m SqlStatementsQuote
sqlQuote = sqlQuoteQueries
  where
    sqlQuoteQueries :: NqlQueries -> ReaderT Int m SqlStatementsQuote
    sqlQuoteQueries (NqlUnion q1 q2) = SqlUnionAll <$> sqlQuoteQueries q1 <*> sqlQuoteQueries q2
    sqlQuoteQueries NqlNil =
      return $
        SqlStatementsSingle $
          SqlSelect
            { sqlSelectFrom = [],
              sqlSelectProj = SqlProjectionNull,
              sqlSelectWhere = [SqlExpBool False]
            }
    sqlQuoteQueries (NqlComprehensionQuerySub q) = SqlStatementsSingle <$> sqlQuoteComprehensions q

    sqlQuoteComprehensions :: NqlComprehensions -> ReaderT Int m SqlStatementQuote
    sqlQuoteComprehensions (NqlFor table body') = do
      withScope
        ( \t -> do
            let alias = table ++ "_" ++ show t
            select <- sqlQuoteComprehensions (body' alias)
            return $
              select
                { sqlSelectFrom = SqlFromTableAliased table alias : sqlSelectFrom select
                }
        )
    sqlQuoteComprehensions (NqlBodyComprehensionSub body) = sqlQuoteBody body

    sqlQuoteBody :: NqlBody -> ReaderT Int m SqlStatementQuote
    sqlQuoteBody (NqlWhere prim cond') =
      (\cond select -> select {sqlSelectWhere = cond : sqlSelectWhere select})
        <$> sqlQuotePrimitives prim
        <*> sqlQuoteBody cond'
    sqlQuoteBody (NqlYield record) = SqlSelect [] <$> sqlQuoteRecord record <*> pure []
    sqlQuoteBody (NqlTable table) =
      pure $
        SqlSelect
          { sqlSelectFrom =
              [SqlFromTable table],
            sqlSelectWhere = [],
            sqlSelectProj = SqlProjectionStar table
          }

    sqlQuoteRecord :: NqlRecord -> ReaderT Int m SqlProjectionsQuote
    sqlQuoteRecord (NqlRecordCon projs) =
      SqlProjectionsAliased
        <$> traverse
          ( \(label, ex) ->
              (,label) <$> sqlQuotePrimitives ex
          )
          projs
    sqlQuoteRecord (NqlVar var) = pure (SqlProjectionStar var)

    sqlQuotePrimitives :: NqlPrimitives -> ReaderT Int m SqlExpressionQuote
    sqlQuotePrimitives (NqlRecordProj t c) = return $ SqlExpProj t c
    sqlQuotePrimitives (NqlFunctionApp "and" [x, y]) = SqlExpOpApp "AND" <$> sqlQuotePrimitives x <*> sqlQuotePrimitives y
    sqlQuotePrimitives (NqlFunctionApp op [x, y]) | not $ all isAlphaNum op = SqlExpOpApp op <$> sqlQuotePrimitives x <*> sqlQuotePrimitives y
    sqlQuotePrimitives (NqlFunctionApp f args) = SqlExpFunApp f <$> mapM sqlQuotePrimitives args
    sqlQuotePrimitives (NqlInt i) = pure $ SqlExpInt i
    sqlQuotePrimitives (NqlBool b) = pure $ SqlExpBool b
    sqlQuotePrimitives (NqlString s) = pure $ SqlExpString s

unquote :: String -> NqlComprehensions -> (String -> NqlComprehensions)
unquote = unquoteComprehension

unquoteComprehension :: String -> NqlComprehensions -> (String -> NqlComprehensions)
unquoteComprehension var (NqlFor table body) val =
  NqlFor table (\var' -> unquoteComprehension var (body var') val)
unquoteComprehension var (NqlBodyComprehensionSub body) val =
  NqlBodyComprehensionSub $ unquoteBody var body val

unquoteBody :: String -> NqlBody -> (String -> NqlBody)
unquoteBody var (NqlWhere cond body) val =
  NqlWhere
    (unquotePrimitives var cond val)
    (unquoteBody var body val)
unquoteBody var (NqlYield record) val =
  NqlYield
    (unquoteRecord var record val)
unquoteBody _var (NqlTable table) _val = NqlTable table

unquoteRecord :: String -> NqlRecord -> (String -> NqlRecord)
unquoteRecord var (NqlRecordCon fields) val =
  NqlRecordCon
    (map (\(label, rhs) -> (label, unquotePrimitives var rhs val)) fields)
unquoteRecord var (NqlVar var') val
  | var == var' =
      NqlVar val
unquoteRecord _ x _ =
  x

unquotePrimitives :: String -> NqlPrimitives -> (String -> NqlPrimitives)
unquotePrimitives var (NqlRecordProj var' field) val | var == var' = NqlRecordProj val field
unquotePrimitives var (NqlFunctionApp fn args) val =
  NqlFunctionApp
    fn
    (map (\p -> unquotePrimitives var p val) args)
unquotePrimitives _ x _ = x

instance Symantics Sql where
  newtype Repr Sql a = ReprSql {unReprSql :: ReaderT Int (Except String) NormalizedQueLam}

  int = ReprSql . return . Primitives . NqlInt
  bool = ReprSql . return . Primitives . NqlBool
  string = ReprSql . return . Primitives . NqlString

  foreach from' body' = ReprSql $ do
    fromUnknown <- unReprSql from'
    withScope \x -> do
      t <- case fromUnknown of
        Body (NqlTable t) -> return t
        _ -> throwError "Unnormalized argument to : `foreach from' body'`: `from'` was not a `table(s)`!"

      bodyUnknown <- unReprSql (body' (ReprSql (return $ Record $ NqlVar (show x))))
      -- 'refine' body:
      bodyComprehension <- case bodyUnknown of
        Comprehensions body -> return body
        Body body -> return (NqlBodyComprehensionSub body)
        _ -> throwError "Unnormalized argument to `foreach from' body'`: `body'` was not of sort Comprehensions!"

      return $ Comprehensions $ NqlFor t (unquote (show x) bodyComprehension)

  yield x' = ReprSql $ do
    xUnknown <- unReprSql x'
    case xUnknown of
      Record x -> return $ Body $ NqlYield x
      _ -> throwError "Unnormalized argument to `yield x'`: `x'` was not of sort Record"

  where_ cond' body' = ReprSql $ do
    condUnknown <- unReprSql cond'
    cond <- case condUnknown of
      Primitives cond -> return cond
      _ -> throwError "Unnormalized argument to `where cond' body''`: `cond'` was not of sort Primitives"

    bodyUnknown <- unReprSql body'
    body <- case bodyUnknown of
      Body body -> return body
      _ -> throwError "Unnormalized argument to `where cond' body''`: `body'` was not of sort Body"

    return $ Body $ NqlWhere cond body

  nil = ReprSql $ return $ Queries NqlNil

  (=%) x' y' = ReprSql $ do
    x <- refinePrimitive x'
    y <- refinePrimitive y'
    return $ Primitives (NqlFunctionApp "=" [x, y])

  and_ x' y' = ReprSql $ do
    x <- refinePrimitive x'
    y <- refinePrimitive y'
    return $ Primitives (NqlFunctionApp "and" [x, y])

  newtype Obs Sql a = ObsSql {unObsSql :: Either String SqlStatementsQuote}

  observe q =
    let q' = runExcept $ flip runReaderT 0 $ do
          q_ <- unReprSql q
          case q_ of
            Queries g -> sqlQuote g
            Comprehensions g -> sqlQuote (NqlComprehensionQuerySub g)
            _ -> throwError "Query not at statement level :-/"
     in ObsSql q'

withScope :: Monad m => (Int -> ReaderT Int m a) -> ReaderT Int m a
withScope binder = do
  x <- ask
  local succ $ binder x
