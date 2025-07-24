import System.IO ( hPutStr, stderr )
import System.Environment ( getArgs )
import Data.Char ( isDigit )

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
        | x `elem` punctuators  =
            let pos = Position col col
            in (Token (TK_PUNCT [x]) pos : ) <$> tokenize' (col+1) xs
        | isDigit x             =
            let pos = Position col (col+len-1)
            in (Token (TK_NUM (read num)) pos : ) <$> tokenize' (col+len) other
        | otherwise             =
            let pos = Position col col
            in Left $ LexError "invalid token" input pos
        where punctuators  = "+-*/()"
              (num, other) = span isDigit s
              len          = length num

data NodeType = ND_ADD
              | ND_SUB
              | ND_MUL
              | ND_DIV
              deriving (Show)

data Node = ND_NUM Int
          | Node
            { nodeType :: NodeType
            , lhs :: Node
            , rhs :: Node
            } deriving (Show)

-- expr = mul ("+" mul | "-" mul)*
parseExpr :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseExpr codes tokens = do
    (left, tokens') <- parseMul codes tokens
    parseExpr' left tokens'
    where
        parseExpr' left [] = Right (left, [])
        parseExpr' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "+" -> do
                (right, tks') <- parseMul codes tks
                parseExpr' (Node ND_ADD left right) tks'
            TK_PUNCT "-" -> do
                (right, tks') <- parseMul codes tks
                parseExpr' (Node ND_SUB left right) tks'
            _ -> Right (left, tk:tks)

-- mul = primary ("*" primary | "/" primary)*
parseMul :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseMul codes tokens = do
    (left, tokens') <- parsePrimary codes tokens
    parseMul' left tokens'
    where
        parseMul' left [] = Right (left, [])
        parseMul' left (tk:tks) = case tokenKind tk of
            TK_PUNCT "*" -> do
                (right, tks') <- parsePrimary codes tks
                parseMul' (Node ND_MUL left right) tks'
            TK_PUNCT "/" -> do
                (right, tks') <- parsePrimary codes tks
                parseMul' (Node ND_DIV left right) tks'
            _ -> Right (left, tk:tks)

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
genExpr (Node ndType l r) = do
    rCode <- genExpr r
    lCode <- genExpr l
    let opCode = case ndType of
            ND_ADD -> "  add %rdi, %rax\n"
            ND_SUB -> "  sub %rdi, %rax\n"
            ND_MUL -> "  imul %rdi, %rax\n"
            ND_DIV -> "  cqo\n" ++
                      "  idiv %rdi\n"
    Right $ rCode ++
            "  push %rax\n" ++
            lCode ++
            "  pop %rdi\n" ++
            opCode