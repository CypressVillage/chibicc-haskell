module CompilerData where

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
    deriving (Show, Eq)

-- AST
data Expr_ a
    = IntLit Int a
    | BinOp BinOp a     (Expr_ a) (Expr_ a)
    | UnaryOp UnaryOp a (Expr_ a)
    | Var LocalVal a
    | Assign a          (Expr_ a) (Expr_ a)
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div
           | Eq | Ne | Lt | Le
    deriving (Show, Eq)

data UnaryOp = Neg | Pos | Addr | DeRef
    deriving (Show, Eq)

data LocalVal = LocalVal
    { name :: String
    , offset :: Int
    } deriving (Show, Eq)

data Stmt_ a
    = ExprStmt     (Expr_ a)
    | ReturnStmt   (Expr_ a)
    | CompoundStmt [Stmt_ a]
    | IfStmt       (Expr_ a) (Stmt_ a) (Maybe (Stmt_ a))
    | ForStmt      (Maybe (Stmt_ a)) (Maybe (Expr_ a)) (Maybe (Expr_ a)) (Stmt_ a)
    deriving (Show, Eq)

type Expr = Expr_ CType
type Stmt = Stmt_ CType

data Function = Function
    { body :: [Stmt]
    , locals :: [LocalVal]
    , stackSize :: Int
    } deriving (Show)