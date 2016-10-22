module WACC.Parser.Parser where

import           Control.Monad
import           Data.Int
import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Error
import           Text.Parsec.Expr

import           WACC.Parser.Types
import           WACC.Parser.Primitives

escape :: GenParser Char st Char
escape = do
    d <- char '\\'
    c <- oneOf escapeCodes
    let i = fromJust $ elemIndex c escapeCodes
    return (escapedCharacters !! i)
    where
      escapeCodes = "\\\"\'0nrvtbf"

nonEscape :: GenParser Char st Char
nonEscape
  = noneOf escapedCharacters

character :: GenParser Char st Char
character
  = nonEscape <|> escape

integer :: GenParser Char st Int32
integer = do
  s <- option id sign
  n <- many1 digit
  return . s . read $ n
  where
    sign = opMap "+" id <|> opMap "-" negate

isKeyword k
  = any (elem k) [keywords, map fst builtins, map fst primTypes]

identifier :: GenParser Char st Identifier
identifier = do
  whitespace
  c <- letter <|> char '_'
  cs <- many identifierChar
  let ident = c : cs
  if isKeyword ident then
    fail "identifier is a keyword"
  else
    return ident

pair :: GenParser Char st a -> GenParser Char st (a, a)
pair p = try $ do
  wschar '('
  p1 <- p
  wschar ','
  p2 <- p
  wschar ')'
  return (p1, p2)

literal :: GenParser Char st Literal
literal
  = charLit <|> intLit <|> boolLit <|> strLit <|> arrLit <|> nullLit
  where
    charLit
      = CHAR <$> try (between (char '\'') (char '\'') character)

    intLit
      = INT <$> try integer

    boolLit
      = BOOL <$> try (reserved "true" True <|> reserved "false" False)

    strLit
      = STR <$> try (quoted (many character))

    arrLit
      = ARRAY <$> try (bracketed (expr `sepBy` comma))

    nullLit
      = try (keyword "null" >> return NULL)


decltype :: GenParser Char st Type
decltype
  = arrType <|> pairType <|> primType
  where
    primType
      = choice (map (uncurry reserved) primTypes)

    arrType = try $ do
      t <- pairType <|> primType
      arrQualifiers <- many1 (reserved "[]" TArray)
      return $ foldr1 (flip (.)) arrQualifiers t

    pairType = try $ do
      keyword "pair"
      (t1, t2) <- option (TArb, TArb) (pair decltype)
      return $ TPair t1 t2

varDecl :: GenParser Char st Declaration
varDecl = try $ do
  t <- wssurrounded decltype
  ident <- wssurrounded identifier
  return (ident, t)

funDecl :: GenParser Char st Declaration
funDecl = try $ do
  retT <- wssurrounded decltype
  ident <- wssurrounded identifier
  args <- parens (varDecl `sepBy` comma)
  return  (ident, TFun retT args)

expr :: GenParser Char st Expr
expr
  = buildExpressionParser operations (wssurrounded term)
  where
    term
      = parens expr <|> funCall <|> newPair <|> val
        <|> arrElement <|> pairElement <|> ident

val :: GenParser Char st Expr
val
  = try $ Lit <$> literal

ident :: GenParser Char st Expr
ident
  = try $ Ident <$> identifier

arrElement :: GenParser Char st Expr
arrElement
  = try $ ArrElem <$> identifier <*> many1 (bracketed expr)

pairElement :: GenParser Char st Expr
pairElement
  = try $ PairElem <$> (reserved "fst" Fst <|> reserved "snd" Snd) <*> expr

funCall :: GenParser Char st Expr
funCall = try $ do
  keyword "call"
  FunCall <$> identifier <*> parens (expr `sepBy` comma)

newPair :: GenParser Char st Expr
newPair = try $ do
  keyword "newpair"
  (e1, e2) <- pair expr
  return $ NewPair e1 e2

stmt :: GenParser Char st Statement
stmt
  = block <|> varDef <|> control <|> cond
    <|> loop <|> builtin <|> noop <|> expStmt

stmtSeq :: GenParser Char st Statement
stmtSeq
  = try $ Block <$> stmt `sepBy` semicolon

noop :: GenParser Char st Statement
noop
  = try $ keyword "skip" *> return Noop

block :: GenParser Char st Statement
block = try $ do
  keyword "begin"
  stmts <- stmt `sepBy` semicolon
  keyword "end"
  return $ Block stmts

varDef :: GenParser Char st Statement
varDef
  = try $ VarDef <$> varDecl <*> (whitespace *> char '=' *> expr)

control :: GenParser Char st Statement
control
  = Ctrl <$> ret -- (ret <|> break <|> cont)
  where
    ret
      = try $ Return <$> (keyword "return" *> expr)

--    break
--      = try $ keyword "break" >> return Break
--
--    cont
--      = try $ keyword "continue" >> return Continue

cond :: GenParser Char st Statement
cond = try $ do
  keyword "if"
  e <- expr
  keyword "then"
  trueBranch <- stmtSeq
  falseBranch <- option Noop (try $ keyword "else" *> stmtSeq)
  keyword "fi"
  return $ Cond e trueBranch falseBranch

loop :: GenParser Char st Statement
loop = try $ do
  keyword "while"
  e <- expr
  keyword "do"
  body <- stmtSeq
  keyword "done"
  return $ Loop e body

builtin :: GenParser Char st Statement
builtin
  = Builtin <$> builtinFunc <*> expr
  where
    builtinFunc
      =  try $ choice (map (uncurry reserved) builtins)

expStmt :: GenParser Char st Statement
expStmt
  = try $ ExpStmt <$> expr

definition :: GenParser Char st Definition
definition
  = try $ (,) <$> funDecl <*> (keyword "is" *> stmtSeq <* keyword "end")

mainDecl :: Declaration
mainDecl
  = ("main", TFun TInt [])

program :: GenParser Char st Program
program = try $ do
  keyword "begin"
  funcs <- many definition
  mainFunc <- stmtSeq
  keyword "end"
  eof
  return $ (mainDecl, mainFunc):funcs
