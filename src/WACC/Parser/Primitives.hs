module WACC.Parser.Primitives where

import           Control.Monad
import           Text.ParserCombinators.Parsec hiding (label)
import           Text.Parsec.Expr
import           WACC.Parser.Types

escapedCharacters
  = "\\\"\'\0\n\r\v\t\b\f"

primTypes
  = [ ("int",    TInt)
    , ("uint8",  TUInt8)
    , ("uint16", TUInt16)
    , ("uint32", TUInt32)
    , ("bool",   TBool)
    , ("char",   TChar)
    , ("string", TString)
    , ("void",   TArb)]

keywords
  = [ "begin"
    , "end"
    , "begin inline"
    , "extern"
    , "struct"
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
  = [ [ postfix "++"   (UnApp PostInc)
      , postfix "--"   (UnApp PostDec)]
    , [ prefix  "len " (UnApp Len)
      , prefix  "ord " (UnApp Ord)
      , prefix  "chr " (UnApp Chr)
      , prefix  "++"   (UnApp PreInc)
      , prefix  "--"   (UnApp PreDec)
      , prefix  "-"    (UnApp Neg)
      , prefix  "!"    (UnApp Not)
      , prefix  "*"    (UnApp Deref)
      , prefix  "&"    (UnApp AddrOf)
      , prefix  "~"    (UnApp BwNot)]
    , [ binary  "."    (BinApp Member)         AssocLeft]
    , [ binary  "*"    (BinApp Mul)            AssocLeft
      , binary  "/"    (BinApp Div)            AssocLeft
      , binary  "%"    (BinApp Mod)            AssocLeft]
    , [ binary  "+"    (BinApp Add)            AssocLeft
      , binary  "-"    (BinApp Sub)            AssocLeft]
    , [ binary  "<<"   (BinApp BwShiftL)       AssocLeft
      , binary  ">>"   (BinApp BwShiftR)       AssocLeft]
    , [ binary  "<="   (BinApp Lte)            AssocLeft
      , binary  ">="   (BinApp Gte)            AssocLeft
      , binary  "<"    (BinApp Lt)             AssocLeft
      , binary  ">"    (BinApp Gt)             AssocLeft]
    , [ binary  "=="   (BinApp Eq)             AssocLeft
      , binary  "!="   (BinApp NEq)            AssocLeft]
    , [ binary  "&"    (BinApp BwAnd)          AssocLeft]
    , [ binary  "^"    (BinApp BwXor)          AssocLeft]
    , [ binary  "|"    (BinApp BwOr)           AssocLeft]
    , [ binary  "&&"   (BinApp And)            AssocLeft]
    , [ binary  "||"   (BinApp Or)             AssocLeft]
    , [ binary  "="    (BinApp Assign)         AssocRight
      , binary  "+="   (BinApp AddAssign)      AssocRight
      , binary  "-="   (BinApp SubAssign)      AssocRight
      , binary  "*="   (BinApp MulAssign)      AssocRight
      , binary  "/="   (BinApp DivAssign)      AssocRight
      , binary  "%="   (BinApp ModAssign)      AssocRight
      , binary  "&="   (BinApp BwAndAssign)    AssocRight
      , binary  "|="   (BinApp BwOrAssign)     AssocRight
      , binary  "^="   (BinApp BwXorAssign)    AssocRight
      , binary  "<<="  (BinApp BwShiftLAssign) AssocRight
      , binary  ">>="  (BinApp BwShiftRAssign) AssocRight]]

-- Utility functions
opMap name ret
  = try (wssurrounded (string name <* notFollowedBy (char '='))) >> return ret

binary name fun
  = Infix $ opMap name fun

prefix name fun
  = Prefix $ opMap name fun

postfix name fun
  = Postfix $ opMap name fun

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

reservedOp name ret
  = try (wsstring name) >> return ret

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
