module Types where

import Data.Int

data Literal
  = CHAR Char
  | INT Int32
  | BOOL Bool
  | STR String
  | ARRAY [Expr]
  | NULL
  deriving (Eq, Show)

-- Can't start with a number
type Identifier
  = String

data PairElement
  = Fst
  | Snd
  deriving (Eq, Show)

data UnOp
  = Not
  | Neg
  | Len
  | Ord
  | Chr
  deriving (Eq, Show)

data BinOp
  = Mul
  | Div
  | Mod
  | Plus
  | Minus
  | Gt
  | Gte
  | Lt
  | Lte
  | Equ
  | NEqu
  | And
  | Or
  deriving (Eq, Show)

data Expr
  = Lit Literal
  | Ident Identifier
  | ArrElem Identifier [Expr]
  | PairElem PairElement Expr
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  | FunCall Identifier [Expr]
  deriving (Eq, Show)

data Type
  = TChar
  | TInt
  | TBool
  | TString
  | TArb String
  | TArray Type
  | TPair Type Type
  | TFun Type [Declaration]
  deriving (Eq, Show)

type Declaration
  = (Identifier, Type)

data BuiltinFunc
  = Read Expr
  | Free Expr
  | Exit Expr
  | Print Expr
  | PrintLn Expr
  | NewPair Expr Expr
  deriving (Eq, Show)

data Control
  = Return Expr
  | Break
  | Continue
  deriving (Eq, Show)

data Statement
  = Noop
  | Block [Statement]
  | Ctrl Control
  | Cond Expr Statement Statement
  | Loop Expr Statement
  | Builtin BuiltinFunc
  | ExpStmt Expr
  deriving (Eq, Show)

type Definition
  = (Declaration, Statement)
