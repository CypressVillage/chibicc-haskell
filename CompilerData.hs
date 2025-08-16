module CompilerData where

-- Error handling
type Code = String

data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   deriving (Show)

-- Token
data Token 
    = TK_PUNCT String
    | TK_NUM Int
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

-- AST
data Expr
    = IntLit Int
    | BinOp BinOp Expr Expr
    | UnaryOp UnaryOp Expr
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
           | Eq | Ne | Lt | Le
    deriving (Show, Eq)

data UnaryOp = Neg | Pos
    deriving (Show, Eq)
