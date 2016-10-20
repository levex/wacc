module Parser where

import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Error
import           Text.Parsec.Expr

import Types

escapedCharacters
  = "\\\"\'\0\n\r\v\t\b\f"

-- Utility functions
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

parens
  = between (char '(') (char ')')

identifierChar
  = alphaNum <|> char '_'

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

identifier :: GenParser Char st Identifier
identifier = do
  whitespace
  c <- letter <|> char '_'
  cs <- many identifierChar
  return $ (c:cs)

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
      = STR <$> try (between (char '"') (char '"') (many character))

    arrLit
      = ARRAY <$> try (between (char '[') (char ']') (expr `sepBy` (char ',')))

    nullLit
      = try (keyword "null" >> return NULL)


decltype :: GenParser Char st Type
decltype = undefined

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
      args <- parens (varDecl `sepBy` (char ','))
      return $ (ident, TFun retT args)

expr :: GenParser Char st Expr
expr
  = Lit <$> literal
