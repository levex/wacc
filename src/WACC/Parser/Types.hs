module WACC.Parser.Types where

import qualified Data.Map as Map
import           Data.Map (Map)

type StatementId
  = Int

data Location = Location
  { row :: Int,
    column :: Int }
  deriving (Eq, Show)

data LocationData = LocationData
  { locations :: Map.Map StatementId Location,
    count :: StatementId }
  deriving (Eq, Show)

data Literal
  = CHAR Char
  | INT Integer
  | BOOL Bool
  | STR String
  | ARRAY [Expr]
  | NULL -- FIXME add TPtr
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
  = Assign
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Gt
  | Gte
  | Lt
  | Lte
  | Eq
  | NEq
  deriving (Eq, Show)

data Expr
  = Lit Literal
  | Ident Identifier
  | ArrElem Identifier [Expr]
  | PairElem PairElement Identifier
  | UnApp UnOp Expr
  | BinApp BinOp Expr Expr
  | FunCall Identifier [Expr]
  | NewPair Expr Expr
  deriving (Eq, Show)

data Type
  = TChar
  | TInt
  | TBool
  | TString
  | TArb
  | TArray Type
  | TPair Type Type
  | TFun Type [Declaration]
  deriving (Eq, Show)

type Declaration
  = (Identifier, Type)

data BuiltinFunc
  = Read
  | Free
  | Exit
  | Print
  | PrintLn
  deriving (Eq, Show)

data Control
  = Return Expr
  | Break
  | Continue
  deriving (Eq, Show)

data Statement
  = Noop
  | Block [Statement]
  | VarDef Declaration Expr
  | Ctrl Control
  | Cond Expr Statement Statement
  | Loop Expr Statement
  | Builtin BuiltinFunc Expr
  | ExpStmt Expr
  | IdentifiedStatement Statement StatementId
  deriving (Eq, Show)

data Definition
  = FunDef Declaration Statement
  deriving (Eq, Show)

type Program
  = [Definition]

type AnnotatedProgram
  = (Program, LocationData)
