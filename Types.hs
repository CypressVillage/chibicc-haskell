module Types where

-- Error Handle
type Code = String
data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   deriving (Show)

data TokenKind = TK_PUNCT String -- keywords or punctuators
               | TK_IDENT String -- identifiers
               | TK_NUM Int      -- numbers
               | TK_EOF          -- end of file
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
data NodeType = ND_ADD -- +
              | ND_SUB -- -
              | ND_MUL -- *
              | ND_DIV -- /
              | ND_EQ  -- ==
              | ND_NE  -- !=
              | ND_LT  -- <
              | ND_LE  -- <=
              deriving (Show)

data Node = ND_EMPTY
          | ND_NUM Int
          | ND_NEG Node
          | ND_EXPR_STMT Node Node
          | ND_OP { nodeType :: NodeType
                  , lhs :: Node
                  , rhs :: Node
                  }
          | ND_ASSIGN LocalVal Node
          | ND_VAR LocalVal
    deriving (Show)

-- Local Variable
data LocalVal = LocalVal
    { name :: String
    , offset :: Int
    }
    deriving (Show)

-- Function
data Function = Function
    { body :: Node
    , localVars :: [LocalVal]
    , stackSize :: Int
    } deriving (Show)