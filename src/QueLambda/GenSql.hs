-- See https://okmij.org/ftp/meta-programming/quel.pdf
module QueLambda.GenSql where

import QueLambda.Symantics
import Control.Monad.State
import Data.List

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
      unexpected -> return unexpected
      -- unexpected -> error $ "Non-normalized argument to 'foreach' table: " ++ show unexpected
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

