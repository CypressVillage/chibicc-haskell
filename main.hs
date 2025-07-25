import System.IO ( hPutStr, stderr )
import System.Environment ( getArgs )
import Data.Char ( isDigit )
import Control.Arrow (ArrowChoice(right))

data TokenKind = TK_PUNCT String
               | TK_NUM Int
               | TK_EOF
               deriving (Show)

data Position = Position
    { startCol :: Int
    , endCol   :: Int
    } deriving (Show)

data Token = Token
    { tokenKind :: TokenKind
    , position  :: Position
    } deriving (Show)

type Code = String
data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   deriving (Show)

main :: IO ()
main = getArgs >>= compile >>= either handleError putStr
    where
        handleError (LexError msg code pos) =
            hPutStr stderr $ formatError msg code pos
        handleError (ParseError msg code pos) =
            hPutStr stderr $ formatError msg code pos

        formatError msg expr pos = unlines
            [ expr
            , replicate (startCol pos - 1) ' ' ++ "^ " ++ msg
            ]

compile :: [String] -> IO (Either CompilerError String)
compile []    = return $ Right ""
compile (codes:_) = pure $ do
    tokens <- tokenize codes
    ast    <- parseExpr codes tokens
    asm    <- genExpr $ fst ast
    return $ unlines
        [ "  .globl main"
        , "main:"
        , asm
        , "  ret"
        ]

tokenize :: String -> Either CompilerError [Token]
tokenize input = tokenize' 1 input where
    tokenize' :: Int -> String -> Either CompilerError [Token]
    tokenize' col []             = Right [Token TK_EOF (Position col col)]
    tokenize' col s@(x:xs)
        | x == ' '              = tokenize' (col+1) xs
        | isPunct x             =
            let pos = Position col col
            in (Token (TK_PUNCT punct) pos : ) <$> tokenize' (col+1) npunct
        | isDigit x             =
            let pos = Position col (col+len-1)
            in (Token (TK_NUM (read num)) pos : ) <$> tokenize' (col+len) nnum
        | otherwise             =
            let pos = Position col col
            in Left $ LexError "invalid token" input pos
        where isPunct         = (`elem` "+-*/()<>=!")
              (punct, npunct) = splitPunct s
              splitPunct ('=':'=':n) = ("==", n)
              splitPunct ('!':'=':n) = ("!=", n)
              splitPunct ('>':'=':n) = (">=", n)
              splitPunct ('<':'=':n) = ("<=", n)
              splitPunct (punct  :n) = ([punct], n)
              (num, nnum)     = span isDigit s
              len             = length num
              

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

-- expr = equality
parseExpr :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseExpr = parseEquality

-- equality = relational ("==" relational | "!=" relational)*
parseEquality :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseEquality codes tokens = do
    (left, tokens') <- parseRelational codes tokens
    parseEquality' left tokens'
    where
        parseEquality' left [] = Right (left, [])
        parseEquality' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "==" -> do
                (right, tks') <- parseRelational codes tks
                parseEquality' (Node ND_EQ left right) tks'
            TK_PUNCT "!=" -> do
                (right, tks') <- parseRelational codes tks
                parseEquality' (Node ND_NE left right) tks'
            _ -> Right (left, tk:tks)

-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*
parseRelational :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseRelational codes tokens = do
    (left, tokens') <- parseAdd codes tokens
    parseRelational' left tokens'
    where
        parseRelational' left [] = Right (left, [])
        parseRelational' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "<" -> do
                (right, tks') <- parseAdd codes tks
                parseRelational' (Node ND_LT left right) tks'
            TK_PUNCT "<=" -> do
                (right, tks') <- parseAdd codes tks
                parseRelational' (Node ND_LE left right) tks'
            TK_PUNCT ">" -> do
                (right, tks') <- parseAdd codes tks
                parseRelational' (Node ND_LT right left) tks'
            TK_PUNCT ">=" -> do
                (right, tks') <- parseAdd codes tks
                parseRelational' (Node ND_LE right left) tks'
            _ -> Right (left, tk:tks)

-- add = mul ("+" mul | "-" mul)*
parseAdd :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseAdd codes tokens = do
    (left, tokens') <- parseMul codes tokens
    parseAdd' left tokens'
    where
        parseAdd' left [] = Right (left, [])
        parseAdd' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "+" -> do
                (right, tks') <- parseMul codes tks
                parseAdd' (Node ND_ADD left right) tks'
            TK_PUNCT "-" -> do
                (right, tks') <- parseMul codes tks
                parseAdd' (Node ND_SUB left right) tks'
            _ -> Right (left, tk:tks)

-- mul = unary ("*" unary | "/" unary)*
parseMul :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseMul codes tokens = do
    (left, tokens') <- parseUnary codes tokens
    parseMul' left tokens'
    where
        parseMul' left [] = Right (left, [])
        parseMul' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "*" -> do
                (right, tks') <- parseUnary codes tks
                parseMul' (Node ND_MUL left right) tks'
            TK_PUNCT "/" -> do
                (right, tks') <- parseUnary codes tks
                parseMul' (Node ND_DIV left right) tks'
            _ -> Right (left, tk:tks)

-- unary = ("+" | "-") unary
--       | primary
parseUnary :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseUnary codes tokens@(tk:rest) = case tokenKind tk of
    TK_PUNCT "+" -> parseUnary codes rest
    TK_PUNCT "-" -> do
        (right, tks) <- parseUnary codes rest
        Right (ND_NEG right, tks)
    _            -> parsePrimary codes tokens

-- primary = "(" expr ")" | num
parsePrimary :: Code -> [Token] -> Either CompilerError (Node, [Token])
parsePrimary codes (tk:rest) = case tokenKind tk of
    TK_PUNCT "(" -> case parseExpr codes rest of
        Left e -> Left e
        Right (node, rParen:rest) -> case tokenKind rParen of
            TK_PUNCT ")" -> Right (node, rest)
            _            -> Left $ ParseError "expected ')'" codes  (position rParen)

    TK_NUM n ->
        Right (ND_NUM n, rest)

    _ -> Left $ ParseError "expected an expression" codes (position tk)

genExpr :: Node -> Either CompilerError String
genExpr (ND_NUM n)        = Right $ "  mov $" ++ show n ++ ", %rax\n"
genExpr (ND_NEG expr)     = (++ "  neg %rax\n") <$> genExpr expr
genExpr (Node ndType l r) = do
    rCode <- genExpr r
    lCode <- genExpr l
    let opCode = case ndType of
            ND_ADD -> "  add %rdi, %rax\n"
            ND_SUB -> "  sub %rdi, %rax\n"
            ND_MUL -> "  imul %rdi, %rax\n"
            ND_DIV -> "  cqo\n"             ++
                      "  idiv %rdi\n"
            ND_EQ  -> "  cmp %rdi, %rax\n"  ++
                      "  sete %al\n"        ++
                      "  movzb %al, %rax\n"
            ND_NE  -> "  cmp %rdi, %rax\n"  ++
                      "  setne %al\n"       ++
                      "  movzb %al, %rax\n"
            ND_LT  -> "  cmp %rdi, %rax\n"  ++
                      "  setl %al\n"        ++
                      "  movzb %al, %rax\n"
            ND_LE  -> "  cmp %rdi, %rax\n"  ++
                      "  setle %al\n"       ++
                      "  movzb %al, %rax\n"
    Right $ rCode ++
            "  push %rax\n" ++
            lCode ++
            "  pop %rdi\n" ++
            opCode