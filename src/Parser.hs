module Parser where

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
    , [ binary  "=="  (BinApp Equ) AssocLeft
      , binary  "!="  (BinApp NEqu) AssocLeft]
    , [ binary  "&&"  (BinApp And) AssocLeft]
    , [ binary  "||"  (BinApp Or) AssocLeft]
    , [ binary  "="   (BinApp Assign) AssocRight]]

-- Utility functions
binary name fun assoc
  = Infix (reserved name fun) assoc

prefix name fun
  = Prefix $ reserved name fun

ignore :: GenParser Char st a -> GenParser Char st ()
ignore p
  = p >> return ()

comment
  = try (string "#" >> manyTill anyChar (try newline))

whitespace
  = (spaces <|> ignore comment) <|> whitespace

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
  = try (string name) >> return ret

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
  let ident = (c:cs)
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
  return $ (p1, p2)

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
      return $ (foldr1 (flip (.)) arrQualifiers) t

    pairType = try $ do
      keyword "pair"
      (t1, t2) <- option (TArb, TArb) (pair decltype)
      return $ TPair t1 t2

decl :: GenParser Char st Declaration
decl
  = funDecl <|> varDecl
  where
    varDecl = try $ do
      t <- wssurrounded decltype
      ident <- wssurrounded identifier
      return $ (ident, t)

    funDecl = try $ do
      retT <- wssurrounded decltype
      ident <- wssurrounded identifier
      args <- parens (varDecl `sepBy` comma)
      return $ (ident, TFun retT args)

expr :: GenParser Char st Expr
expr
  = buildExpressionParser operations (wssurrounded term)
  where
    term
      = parens expr <|> val <|> arrElement <|> pairElement <|> lVal

val :: GenParser Char st Expr
val
  = Lit <$> literal

arrElement :: GenParser Char st Expr
arrElement
  = try $ ArrElem <$> identifier <*> many1 (bracketed expr)

pairElement :: GenParser Char st Expr
pairElement
  = try $ PairElem <$> (reserved "fst" Fst <|> reserved "snd" Snd) <*> expr

lVal :: GenParser Char st Expr
lVal
  = try $ Ident <$> identifier
