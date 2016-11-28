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
  | PreInc
  | PostInc
  | PreDec
  | PostDec
  | Deref
  | AddrOf
  | BwNot
  deriving (Eq, Show)

data BinOp
  = Add
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
  | Member
  | BwAnd
  | BwOr
  | BwXor
  | BwShiftL
  | BwShiftR
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | BwAndAssign
  | BwOrAssign
  | BwXorAssign
  | BwShiftLAssign
  | BwShiftRAssign
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
  | NewStruct Identifier
  deriving (Eq, Show)

data Type
  = TChar
  | TInt
  | TBool
  | TString
  | TArb
  | TArray Type
  | TPair Type Type
  | TPtr Type
  | TStruct Identifier
  | TFun Type [Declaration]
  deriving (Eq, Show)

type Declaration
  = (Identifier, Type)

type Offset = Int

data BuiltinFunc
  = Read
  | Free
  | Exit
  | Print
  | PrintLn
  deriving (Eq, Show)

data Control
  = Return (Maybe Expr)
  | Break
  | Continue
  deriving (Eq, Show)

data Statement
  = Noop
  | Block [Statement]
  | InlineAssembly [String]
  | ExternDecl Identifier
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
  | TypeDef Identifier [Declaration]
  | GlobalDef Declaration Expr
  deriving (Eq, Show)

type Program
  = [Definition]

type AnnotatedProgram
  = (Program, LocationData)

arithmOps :: [BinOp]
arithmOps
  = [ Add
    , Sub
    , Mul
    , Div
    , Mod
    , Member
    , BwAnd
    , BwOr
    , BwXor
    , BwShiftL
    , BwShiftR
    ]

booleanOps :: [BinOp]
booleanOps
  = [ And
    , Or
    , Gt
    , Gte
    , Lt
    , Lte
    , Eq
    , NEq
    ]

assignmentOps :: [BinOp]
assignmentOps
  = [ AddAssign
    , SubAssign
    , MulAssign
    , DivAssign
    , ModAssign
    , BwAndAssign
    , BwOrAssign
    , BwXorAssign
    , BwShiftLAssign
    , BwShiftRAssign
    ]

getDecl (FunDef d _)
  = d
getDecl (TypeDef i _)
  = (i, TStruct i)
getDecl (GlobalDef d _)
  = d

isFunDef :: Definition -> Bool
isFunDef (FunDef _ _) = True
isFunDef _ = False
