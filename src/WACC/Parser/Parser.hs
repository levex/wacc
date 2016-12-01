module WACC.Parser.Parser where

import           Control.Monad
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import           Data.Map (Map)
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Pos

import           WACC.Parser.Types
import           WACC.Parser.Primitives
import           WACC.Semantics.Typing

initialParserState :: LocationData
initialParserState
  = LocationData Map.empty 0


getNextIdentifier :: GenParser Char LocationData StatementId
getNextIdentifier = do
  LocationData locs c <- getState
  setState (LocationData locs (c+1))
  return c

savePosition :: StatementId -> GenParser Char LocationData ()
savePosition i = do
  LocationData locs c <- getState
  pos <- getPosition
  let newLocs = Map.insert i (Location (sourceLine pos) (sourceColumn pos)) locs
  setState (LocationData newLocs c)

-- Parsing
escape :: GenParser Char LocationData Char
escape = do
    d <- char '\\'
    c <- oneOf escapeCodes
    let i = fromJust $ elemIndex c escapeCodes
    return (escapedCharacters !! i)
    where
      escapeCodes = "\\\"\'0nrvtbf"

nonEscape :: GenParser Char LocationData Char
nonEscape
  = noneOf escapedCharacters

character :: GenParser Char LocationData Char
character
  = nonEscape <|> escape

number :: Integer -> GenParser Char LocationData Char -> GenParser Char LocationData Integer
number base baseDigit = try $ do
  digits <- many1 baseDigit
  return $ foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits

integer :: GenParser Char LocationData Integer
integer
  = hexadecimal <|> decimal
  where
    decimal = number 10 digit
    hexadecimal = try $ string "0x" *> number 16 hexDigit

isKeyword :: String -> Bool
isKeyword k
  = any (elem k) [keywords, map fst builtins, map fst primTypes]

identifier :: GenParser Char LocationData Identifier
identifier = do
  whitespace
  c <- letter <|> char '_'
  cs <- many identifierChar
  let ident = c : cs
  if isKeyword ident then
    fail "identifier is a keyword"
  else
    return ident

pair :: GenParser Char LocationData a -> GenParser Char LocationData (a, a)
pair p = try $ do
  wschar '('
  p1 <- p
  wschar ','
  p2 <- p
  wschar ')'
  return (p1, p2)

literal :: GenParser Char LocationData Literal
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


decltype :: GenParser Char LocationData Type
decltype = do
  t <- structType <|> arrType <|> primType
  qualifiers <- many ptrQualifier
  return $ foldr (flip (.)) id qualifiers t
  where
    primType
      = choice (map (uncurry reserved) primTypes)

    arrType = try $ do
      t <- pairType <|> primType
      qualifiers <- many1 (arrQualifier <|> ptrQualifier)
      return $ foldr1 (flip (.)) qualifiers t

    arrQualifier
      = reservedOp "[]" TArray

    ptrQualifier
      = reservedOp "*" TPtr

    pairType = try $ do
      keyword "pair"
      (t1, t2) <- option (TArb, TArb) (pair decltype)
      return $ TPair t1 t2

    structType
      = try $ TStruct <$> (keyword "struct" *> identifier)

varDecl :: GenParser Char LocationData Declaration
varDecl = try $ do
  t <- wssurrounded decltype
  ident <- wssurrounded identifier
  return (ident, t)

funDecl :: GenParser Char LocationData Declaration
funDecl = try $ do
  retT <- wssurrounded decltype
  ident <- wssurrounded identifier
  args <- parens (varDecl `sepBy` comma)
  return  (ident, TFun retT args)

expr :: GenParser Char LocationData Expr
expr
  = buildExpressionParser operations (wssurrounded term)
  where
    term
      = parens expr <|> funCall <|> newPair <|> sizeOf
        <|> val <|> arrElement <|> pairElement <|> ident

val :: GenParser Char LocationData Expr
val
  = try $ Lit <$> literal

ident :: GenParser Char LocationData Expr
ident = try $ Ident <$> identifier

arrElement :: GenParser Char LocationData Expr
arrElement
  = try $ ArrElem <$> identifier <*> many1 (bracketed expr)

pairElement :: GenParser Char LocationData Expr
pairElement
  = try $ PairElem <$> (reserved "fst" Fst <|> reserved "snd" Snd)
                   <*> identifier

funCall :: GenParser Char LocationData Expr
funCall = try $ do
  keyword "call"
  FunCall <$> identifier <*> (whitespace *> parens (expr `sepBy` comma))

newPair :: GenParser Char LocationData Expr
newPair = try $ do
  keyword "newpair"
  (e1, e2) <- pair expr
  return $ NewPair e1 e2

sizeOf :: GenParser Char LocationData Expr
sizeOf = try $ do
  keyword "sizeof"
  SizeOf <$> parens decltype

stmt :: GenParser Char LocationData Statement
stmt
  = inlineAsm <|> block <|> varDef <|> control <|> cond
    <|> loop <|> builtin <|> noop <|> expStmt

idStmt :: GenParser Char LocationData Statement
idStmt = do
  i <- getNextIdentifier
  savePosition i
  s <- stmt
  return $ IdentifiedStatement s i

noop :: GenParser Char LocationData Statement
noop
  = try $ reserved "skip" Noop <* semicolon

inlineAsm :: GenParser Char LocationData Statement
inlineAsm = try $ do
  keyword "begin inline"
  ss <- manyTill anyChar (try $ keyword "end")
  return $ InlineAssembly [ss, "\n"]

block :: GenParser Char LocationData Statement
block = try $ do
  keyword "begin"
  stmts <- many idStmt
  keyword "end"
  return $ Block stmts

varDef :: GenParser Char LocationData Statement
varDef
  = try $ VarDef <$> varDecl <*> (whitespace *> char '=' *> expr <* semicolon)

control :: GenParser Char LocationData Statement
control
  = Ctrl <$> (ret <|> break <|> cont)
  where
    ret
      = reserved "return" Return <*> optionMaybe expr <* semicolon

    break
      = reserved "break" Break <* semicolon

    cont
      = reserved "continue" Continue <* semicolon

cond :: GenParser Char LocationData Statement
cond = try $ do
  keyword "if"
  e <- expr
  keyword "then"
  trueBranch <- Block <$> many idStmt
  falseBranch <- option Noop (try $ keyword "else" *> (Block <$> many idStmt))
  keyword "fi" <|> keyword "end"
  return $ Cond e trueBranch falseBranch

loop :: GenParser Char LocationData Statement
loop
  = whileLoop <|> forLoop
  where
    forLoop = try $ do
      keyword "for"
      i <- getNextIdentifier
      savePosition i
      init <- varDef
      semicolon
      cond <- expr
      semicolon
      step <- expStmt
      keyword "do"
      body <- Block <$> many idStmt
      keyword "done" <|> keyword "end"
      return $ Block [IdentifiedStatement init i,
        IdentifiedStatement (Loop cond (Block [IdentifiedStatement body i,
          IdentifiedStatement step i])) i]

    whileLoop = try $ do
      keyword "while"
      e <- expr
      keyword "do"
      body <- Block <$> many idStmt
      keyword "done" <|> keyword "end"
      return $ Loop e body

builtin :: GenParser Char LocationData Statement
builtin
  = Builtin <$> builtinFunc <*> expr <* semicolon
  where
    builtinFunc
      =  try $ choice (map (uncurry reserved) builtins)

expStmt :: GenParser Char LocationData Statement
expStmt
  = try $ ExpStmt <$> expr <* semicolon

definition :: GenParser Char LocationData Definition
definition
  = externDef <|> funDef <|> structDef <|> globalDef
  where
    funDef
      = try $ FunDef <$> funDecl <*> (keyword "is" *> (Block <$> many idStmt) <* keyword "end")

    structDef
      = try $ TypeDef <$> (keyword "struct" *> identifier <* keyword "is")
                      <*> (varDecl `sepBy` semicolon <* keyword "end")

    globalDef
      = try $ GlobalDef <$> varDecl <*> (whitespace *> char '=' *> expr)

    externDef
      = try $ ExternDef <$> (keyword "extern" *> (funDecl <|> varDecl) <* semicolon)

program :: GenParser Char LocationData AnnotatedProgram
program = try $ do
  keyword "begin"
  defs <- many definition
  keyword "end"
  eof
  st <- getState
  return $ (defs, st)
