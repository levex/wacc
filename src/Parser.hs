module Parser where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Error
import           Text.Parsec.Expr

import Types

escapedCharacters
  = "\\\"\'\0\n\r\v\t\b\f"

primTypes
  = [ ("int",    TInt)
    , ("bool",   TBool)
    , ("char",   TChar)
    , ("string", TString)]

keywords
  = [ "begin"
    , "end"
    , "is"
    , "skip"
    , "if"
    , "then"
    , "else"
    , "fi"
    , "while"
    , "do"
    , "done"
    , "return"
    , "newpair"
    , "call"
    , "fst"
    , "snd"
    , "pair"
    , "null"
    , "true"
    , "false"]

builtins
  = [ ("read", Read)
    , ("free", Free)
    , ("exit", Exit)
    , ("print", Print)
    , ("println", PrintLn)]

operations
  = [ [ prefix  "len" (UnApp Len)
      , prefix  "ord" (UnApp Ord)
      , prefix  "chr" (UnApp Chr)
      , prefix  "-"   (UnApp Neg)
      , prefix  "!"   (UnApp Not)]
    , [ binary  "*"   (BinApp Mul) AssocLeft
      , binary  "/"   (BinApp Div) AssocLeft
      , binary  "%"   (BinApp Mod) AssocLeft]
    , [ binary  "+"   (BinApp Add) AssocLeft
      , binary  "-"   (BinApp Sub) AssocLeft]
    , [ binary  "<"   (BinApp Lt) AssocLeft
      , binary  ">"   (BinApp Gt) AssocLeft
      , binary  "<="  (BinApp Lte) AssocLeft
      , binary  ">="  (BinApp Gte) AssocLeft]
    , [ binary  "=="  (BinApp Eq) AssocLeft
      , binary  "!="  (BinApp NEq) AssocLeft]
    , [ binary  "&&"  (BinApp And) AssocLeft]
    , [ binary  "||"  (BinApp Or) AssocLeft]
    , [ binary  "="   (BinApp Assign) AssocRight]]

-- Utility functions
opMap name ret
  = try (string name) >> return ret

binary name fun
  = Infix (opMap name fun)

prefix name fun
  = Prefix $ opMap name fun

ignore :: GenParser Char st a -> GenParser Char st ()
ignore
  = void

comment
  = try $ char '#' >> manyTill anyChar newline

spaces1
  = space >> spaces

whitespace
  = skipMany (spaces1 <|> ignore comment)

wssurrounded p
  = whitespace *> p <* whitespace

wschar
  = wssurrounded . char

wsstring
  = wssurrounded . string

keyword s
  = wssurrounded (string s <* notFollowedBy identifierChar)

semicolon
  = wschar ';'

comma
  = wschar ','

reserved name ret
  = try (keyword name) >> return ret

parens
  = between (char '(') (char ')')

bracketed
  = between (char '[') (char ']')

quoted
  = between (char '"') (char '"')

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

isKeyword k
  = any (elem k) [keywords, map fst builtins, map fst primTypes]

identifierChar
  = alphaNum <|> char '_'

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
      = INT . read <$> try (many1 digit)

    boolLit
      = BOOL <$> try ((keyword "true" >> return True)
                 <|> (keyword "false" >> return False))

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
  = Lit <$> literal

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
  = Ctrl <$> (ret <|> break <|> cont)
  where
    ret
      = try $ Return <$> (keyword "return" *> expr)

    break
      = try $ keyword "break" >> return Break

    cont
      = try $ keyword "continue" >> return Continue

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

program :: GenParser Char st Program
program = try $ do
  keyword "begin"
  funcs <- many definition
  mainFunc <- stmtSeq
  keyword "end"
  eof
  return $ (mainDecl, mainFunc):funcs

mainDecl :: Declaration
mainDecl
  = ("main", TFun TInt [])

runParser :: String -> String -> Either ParseError Program
runParser fileName contents
  = parse program fileName contents
