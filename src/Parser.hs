module Parser where

import           Text.ParserCombinators.Parsec hiding (label)
import           Text.ParserCombinators.Parsec.Error
import           Text.Parsec.Expr

import Types

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
      = CHAR <$> try (between (char '\'') (char '\'') anyChar)

    intLit
      = INT . read <$> try (many1 digit)

    boolLit
      = undefined -- BOOL . read <$> try (string "true" <|> string "false")

    strLit
      = undefined

    arrLit
      = undefined

    nullLit
      = try (string "null" >> return NULL)


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
