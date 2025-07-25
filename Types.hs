module Types where

-- Error Handle
type Code = String
data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   deriving (Show)

data TokenKind = TK_PUNCT String
               | TK_NUM Int
               | TK_EOF
               deriving (Show)

-- Token
data Position = Position
    { startCol :: Int
    , endCol   :: Int
    } deriving (Show)

data Token = Token
    { tokenKind :: TokenKind
    , position  :: Position
    } deriving (Show)

-- AST
data NodeType = ND_ADD
              | ND_SUB
              | ND_MUL
              | ND_DIV
              | ND_EQ
              | ND_NE
              | ND_LT
              | ND_LE
              deriving (Show)

data Node = ND_NUM Int
          | ND_NEG Node
          | Node
            { nodeType :: NodeType
            , lhs :: Node
            , rhs :: Node
            } deriving (Show)