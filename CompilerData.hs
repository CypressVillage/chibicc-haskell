{-# LANGUAGE DeriveDataTypeable #-}
module CompilerData where

import Data.Data
import Data.Generics.Uniplate.Data

-- Error handling
type Code = String

data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   | SemanticError String
                   deriving (Show)

-- Token
data Token 
    = TK_PUNCT String
    | TK_IDENT String
    | TK_NUM Int
    | TK_KEYWORD String
    | TK_EOF
    deriving (Show, Eq)

data Position = Position
    { startCol :: Int
    , endCol   :: Int
    } deriving (Show)

data PosToken = PosToken
    { token    :: Token
    , position :: Position
    } deriving (Show)

-- Type
data CType 
    = CNaN
    | CInt
    | CPtr CType
    deriving (Show, Eq, Data, Typeable)

-- AST
data Expr
    = IntLit  Int      CType 
    | BinOp   BinOp    CType Expr Expr
    | UnaryOp UnaryOp  CType Expr
    | Var     LocalVal CType
    | Assign           CType Expr Expr
    deriving (Show, Eq, Data, Typeable)

data BinOp = Add | Sub | Mul | Div
           | Eq | Ne | Lt | Le
    deriving (Show, Eq, Data, Typeable)

data UnaryOp = Neg | Pos | Addr | DeRef
    deriving (Show, Eq, Data, Typeable)

data LocalVal = LocalVal
    { name :: String
    , offset :: Int
    } deriving (Show, Eq, Data, Typeable)

data Stmt
    = ExprStmt     Expr
    | ReturnStmt   Expr
    | CompoundStmt [Stmt]
    | IfStmt       Expr Stmt (Maybe Stmt)
    | ForStmt      (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt
    deriving (Show, Eq, Data, Typeable)

data Function = Function
    { body :: [Stmt]
    , locals :: [LocalVal]
    , stackSize :: Int
    } deriving (Show)

type Name = String

data Decl
    = VarDecl CType Name (Maybe Expr)