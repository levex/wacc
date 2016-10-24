module WACC.Parser.Primitives where

import           Control.Monad
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.Parsec.Expr
import           WACC.Parser.Types

escapedCharacters
  = "\\\"\'\0\n\r\v\t\b\f"

primTypes
  = [ ("int",    TInt)
    , ("bool",   TBool)
    , ("char",   TChar)
    , ("string", TString)]

unOpTypes
  = [ (Not, (TBool, TBool))
    , (Neg, (TInt, TInt))
    , (Len, (TArray TArb, TInt))
    , (Ord, (TChar, TInt))
    , (Chr, (TInt, TChar))
    ]

-- FIXME: investigate whether Add/Sub/Mul/Div/Mod/And/Or take TChar's too.
-- FIXME: Eq and NEq should work for other types
-- FIXME: Assign is one weird hack, also is return type good?
binAppTypes
  = [ (Assign, (TArb, TArb, TBool))
    , (Add, (TInt, TInt, TInt))
    , (Sub, (TInt, TInt, TInt))
    , (Mul, (TInt, TInt, TInt))
    , (Div, (TInt, TInt, TInt))
    , (Mod, (TInt, TInt, TInt))
    , (And, (TInt, TInt, TInt))
    , (Or, (TInt, TInt, TInt))
    , (Gt, (TInt, TInt, TBool))
    , (Gte, (TInt, TInt, TBool))
    , (Lt, (TInt, TInt, TBool))
    , (Lte, (TInt, TInt, TBool))
    , (Eq, (TInt, TInt, TBool))
    , (NEq, (TInt, TInt, TBool))
    ]

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
    , [ binary  "*"   (BinApp Mul)    AssocLeft
      , binary  "/"   (BinApp Div)    AssocLeft
      , binary  "%"   (BinApp Mod)    AssocLeft]
    , [ binary  "+"   (BinApp Add)    AssocLeft
      , binary  "-"   (BinApp Sub)    AssocLeft]
    , [ binary  "<="  (BinApp Lte)    AssocLeft
      , binary  ">="  (BinApp Gte)    AssocLeft
      , binary  "<"   (BinApp Lt)     AssocLeft
      , binary  ">"   (BinApp Gt)     AssocLeft]
    , [ binary  "=="  (BinApp Eq)     AssocLeft
      , binary  "!="  (BinApp NEq)    AssocLeft]
    , [ binary  "&&"  (BinApp And)    AssocLeft]
    , [ binary  "||"  (BinApp Or)     AssocLeft]
    , [ binary  "="   (BinApp Assign) AssocRight]]

-- Utility functions
opMap name ret
  = try (wsstring name) >> return ret

binary name fun
  = Infix $ opMap name fun

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

identifierChar
  = alphaNum <|> char '_'

keyword s
  = wssurrounded (string s <* notFollowedBy identifierChar)

semicolon
  = wschar ';'

comma
  = wschar ','

reserved name ret
  = try (keyword name) >> return ret

parens :: GenParser Char st a -> GenParser Char st a
parens
  = between (char '(') (char ')')

bracketed :: GenParser Char st a -> GenParser Char st a
bracketed
  = between (char '[') (char ']')

quoted :: GenParser Char st a -> GenParser Char st a
quoted
  = between (char '"') (char '"')
