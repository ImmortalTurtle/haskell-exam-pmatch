module PMParser where

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char (string)
import Text.Parsec.Token (reservedOp)
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)
--import ULC (Info, infoFrom, parens )


data Info = Info { row :: Int, col :: Int } deriving (Show)
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)


type Parser rez = Parsec String () rez

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

stringConst :: String -> Parser ()
stringConst s = do
  _ <- string s
  return ()

data BinOpSort = Mul | Add | Sub | Eq | LessThen | LessEq deriving Show
data Ops a = TermConstruction
  { int    :: Info -> Int -> a
  , tag    :: Info -> String -> a
  , var    :: Info -> String -> a
  , field  :: Info -> Int -> a -> a
  , binop  :: BinOpSort -> a -> a -> a
  , econstr:: Info -> String -> [a] -> a
  , ifthenelse :: Info -> a -> a -> a -> a
  }

decimal :: Ops a -> Parser a
decimal ops = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ int ops (infoFrom pos) n)

parseRhs ops = runParser (parserRhs ops) () "RHS of match"

parserRhs ops =
  buildExpressionParser (table ops)
    (spaces *> (block ops) <* spaces)
  <?> "expression"
  where
    table ops =
      [ [binary "*" (binop ops Mul) AssocLeft ]
      , [binary "+" (binop ops Add) AssocLeft, binary "-" (binop ops Sub) AssocLeft ]
      , [binary "=" (binop ops Eq)  AssocNone, binary "<" (binop ops LessThen) AssocNone, binary "<=" (binop ops LessEq) AssocNone ]
      ]
    binary name fun assoc =
      Infix (do{ spaces *> (stringConst name); return fun }) assoc
    block ops = parens (parserRhs ops)
            <|> ifthenelseParser ops
            <|> decimal ops
            <|> fieldOp ops
            <|> tagOp ops
            <|> varName ops

    ifthenelseParser ops = do
      spaces
      stringConst "if"
      spaces
      cond_ <- parserRhs ops
      spaces
      stringConst "then"
      spaces
      then_ <- parserRhs ops
      spaces
      stringConst "else"
      spaces
      else_ <- parserRhs ops
      pos <- getPosition
      spaces
      return $ ifthenelse ops (infoFrom pos) cond_ then_ else_
    fieldOp ops = do
      spaces
      stringConst "field"
      spaces
      digits <- many1 digit
      let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
      spaces
      v <- parserRhs ops
      pos <- getPosition
      spaces
      return $ field ops (infoFrom pos) n v

    --tagOp :: Stream s m t => Ops a -> ParsecT s () m a
    tagOp :: Ops a -> Parser a
    tagOp ops = do
      spaces
      string "tag"
      spaces
      v <- many1 letter
      pos <- getPosition
      spaces
      return $ tag ops (infoFrom pos) v

    varName :: Ops a -> Parser a
    varName ops = do
      spaces
      v <- many1 letter
      pos <- getPosition
      spaces
      return $ var ops (infoFrom pos) v

{- ----------------------------------------------------------------- -}
commaSep :: Parsec String () a -> Parsec String () [a]
commaSep p  = p `sepBy` (string ",")
lident = many1 letter
uident = do
  u <- upper
  us <- many letter
  return (u:us)
{------------------------  parsing scrutinee------------------------ -}
parseScrutinee ops = runParser (parserScrutinee ops) () "scrutinee"

parserScrutinee ops = pNum ops <|> pConstr ops
  where
    pNum = decimal
    pConstr ops = do
      name <- uident
      args <- option [] (parens (commaSep $ parserScrutinee ops))
      pos <- getPosition
      return $ econstr ops (infoFrom pos) name args
{------------------------  parsing patterns --------------------------}
data PattOps a = PattConstruction
  { wild    :: Info -> a
  , pconstr :: Info -> String -> [a] -> a
  , named   :: Info -> String -> a
  , pconst  :: Info -> Int -> a
  }

parsePatt ops = runParser (parserPatt ops) () "pattern"
parserPatt :: PattOps a -> Parsec String () a
parserPatt ops = (pConstr ops) <|> (pVar ops) <|> (pWild ops) <|> (pInt ops)
  where
    -- TODO: support spaces
    pConstr ops = do
      name <- uident
      args <- option [] (parens (commaSep $ parserPatt ops))
      pos <- getPosition
      return $ pconstr ops (infoFrom pos) name args
    pVar ops = do
      v <- lident
      pos <- getPosition
      return $ named ops (infoFrom pos) v
    pWild :: PattOps a -> Parser a
    pWild ops = do
      string "_"
      pos <- getPosition
      return $ wild ops (infoFrom pos)
    pInt :: PattOps a -> Parser a
    pInt ops = do
      digits <- many1 digit
      pos <- getPosition
      let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
      spaces
      return $ pconst ops (infoFrom pos) n

{- -------------------- parse case ------------------------------------- -}
parseCase opsP opsE = runParser (parserCase opsP opsE) () "Case"
parseCases opsP opsE = runParser (many $ parserCase opsP opsE ) () "Cases"

parserCase opsP opsE = do
  lhs <- parserPatt opsP
  spaces
  string "->"
  spaces
  rhs <- parserRhs opsE
  return (lhs,rhs)
