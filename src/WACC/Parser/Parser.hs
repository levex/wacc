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

-- our state is a symbol table
type UState = [Symbol]

escape :: GenParser Char UState Char
escape = do
    d <- char '\\'
    c <- oneOf escapeCodes
    let i = fromJust $ elemIndex c escapeCodes
    return (escapedCharacters !! i)
    where
      escapeCodes = "\\\"\'0nrvtbf"

nonEscape :: GenParser Char UState Char
nonEscape
  = noneOf escapedCharacters

character :: GenParser Char UState Char
character
  = nonEscape <|> escape

integer :: GenParser Char UState Int32
integer = do
  s <- option id sign
  n <- many1 digit
  return . s . read $ n
  where
    sign = opMap "+" id <|> opMap "-" negate

isKeyword :: String -> Bool
isKeyword k
  = any (elem k) [keywords, map fst builtins, map fst primTypes]

identifier :: GenParser Char UState Identifier
identifier = do
  whitespace
  c <- letter <|> char '_'
  cs <- many identifierChar
  let ident = c : cs
  if isKeyword ident then
    fail "identifier is a keyword"
  else
    return ident

pair :: GenParser Char UState a -> GenParser Char UState (a, a)
pair p = try $ do
  wschar '('
  p1 <- p
  wschar ','
  p2 <- p
  wschar ')'
  return (p1, p2)

literal :: GenParser Char UState Literal
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


decltype :: GenParser Char UState Type
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

varDecl :: GenParser Char UState Declaration
varDecl = try $ do
  t <- wssurrounded decltype
  ident <- wssurrounded identifier
  return (ident, t)

funDecl :: GenParser Char UState Declaration
funDecl = try $ do
  retT <- wssurrounded decltype
  ident <- wssurrounded identifier
  args <- parens (varDecl `sepBy` comma)
  return  (ident, TFun retT args)

expr :: GenParser Char UState Expr
expr
  = buildExpressionParser operations (wssurrounded term)
  where
    term
      = parens expr <|> funCall <|> newPair <|> val
        <|> arrElement <|> pairElement <|> ident

val :: GenParser Char UState Expr
val
  = try $ Lit <$> literal

ident :: GenParser Char UState Expr
ident
  = try $ Ident <$> identifier

arrElement :: GenParser Char UState Expr
arrElement
  = try $ ArrElem <$> identifier <*> many1 (bracketed expr)

pairElement :: GenParser Char UState Expr
pairElement
  = try $ PairElem <$> (reserved "fst" Fst <|> reserved "snd" Snd) <*> expr

funCall :: GenParser Char UState Expr
funCall = try $ do
  keyword "call"
  FunCall <$> identifier <*> parens (expr `sepBy` comma)

newPair :: GenParser Char UState Expr
newPair = try $ do
  keyword "newpair"
  (e1, e2) <- pair expr
  return $ NewPair e1 e2

stmt :: GenParser Char UState Statement
stmt
  = block <|> varDef <|> control <|> cond
    <|> loop <|> builtin <|> noop <|> expStmt

stmtSeq :: GenParser Char UState Statement
stmtSeq
  = try $ Block <$> stmt `sepBy` semicolon

noop :: GenParser Char UState Statement
noop
  = try $ keyword "skip" *> return Noop

block :: GenParser Char UState Statement
block = try $ do
  keyword "begin"
  stmts <- stmt `sepBy` semicolon
  keyword "end"
  return $ Block stmts

varDef :: GenParser Char UState Statement
varDef
  = try $ VarDef <$> varDecl <*> (whitespace *> char '=' *> expr)

control :: GenParser Char UState Statement
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

cond :: GenParser Char UState Statement
cond = try $ do
  keyword "if"
  e <- expr
  keyword "then"
  trueBranch <- stmtSeq
  keyword "else"
  falseBranch <- stmtSeq
  keyword "fi"
  return $ Cond e trueBranch falseBranch

loop :: GenParser Char UState Statement
loop = try $ do
  keyword "while"
  e <- expr
  keyword "do"
  body <- stmtSeq
  keyword "done"
  return $ Loop e body

builtin :: GenParser Char UState Statement
builtin
  = Builtin <$> builtinFunc <*> expr
  where
    builtinFunc
      =  try $ choice (map (uncurry reserved) builtins)

expStmt :: GenParser Char UState Statement
expStmt
  = try $ ExpStmt <$> expr

definition :: GenParser Char UState Definition
definition
  = try $ (,) <$> funDecl <*> (keyword "is" *> stmtSeq <* keyword "end")

mainDecl :: Declaration
mainDecl
  = ("main", TFun TInt [])

program :: GenParser Char UState Program
program = try $ do
  keyword "begin"
  funcs <- many definition
  notFollowedBy $ keyword "end"
  mainFunc <- stmtSeq
  keyword "end"
  eof
  return $ (mainDecl, mainFunc):funcs
