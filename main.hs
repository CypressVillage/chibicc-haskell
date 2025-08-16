{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import System.IO ( hPutStr, stderr )
import System.Environment ( getArgs )
import Data.Char ( isDigit )
import Data.Functor (($>))
import Control.Monad (void)
import Control.Monad.Except (Except, ExceptT, throwError, catchError, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), runReaderT, ask)
import Control.Applicative (Alternative (many), empty, (<|>))

data CompilerError = LexError String Code Position
                   | ParseError String Code Position
                   deriving (Show)

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

tokenize :: String -> Either CompilerError [PosToken]
tokenize input = tokenize' 1 input where
    tokenize' :: Int -> String -> Either CompilerError [PosToken]
    tokenize' col []             = Right [PosToken TK_EOF (Position col col)]
    tokenize' col s@(x:xs)
        | x == ' '              = tokenize' (col+1) xs
        | isPunct x             =
            let pos = Position col col
            in (PosToken (TK_PUNCT punct) pos : ) <$> tokenize' (col+1) npunct
        | isDigit x             =
            let pos = Position col (col+len-1)
            in (PosToken (TK_NUM (read num)) pos : ) <$> tokenize' (col+len) nnum
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

type Code = String
type ParserEnv a = ReaderT Code (ExceptT CompilerError Identity) a
newtype Parser a = Parser { runParser :: [PosToken] -> ParserEnv (a, [PosToken]) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \tokens -> do
        (result, rest) <- p tokens
        return (f result, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \tokens -> return (x, tokens)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser pa = Parser $ \tokens -> do
        (f, rest1) <- pf tokens
        (a, rest2) <- pa rest1
        return (f a, rest2)

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= f = Parser $ \tokens -> do
        (result, rest) <- p tokens
        runParser (f result) rest

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> do
        code <- ask
        throwError $ ParseError "No valid parse" code (Position 0 0)

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \tokens ->
        runParser p1 tokens `catchError` \_ -> runParser p2 tokens

-- error handling
throwParserError :: String -> Parser a
throwParserError msg = Parser $ \_ -> do
    code <- ask
    throwError $ ParseError msg code (Position 0 0)

(<?>) :: Parser a -> String -> Parser a
p <?> msg = Parser $ \tokens -> do
    (result, rest) <- runParser p tokens
    return (result, rest) `catchError` \_ -> do
        code <- ask
        throwError $ ParseError msg code (position $ head tokens)

anyToken :: Parser PosToken
anyToken = Parser $ \tokens -> case tokens of
    [] -> do
        code <- ask
        throwError $ ParseError "msg" code (Position 0 0)
    (t:ts) -> return (t, ts)

peekToken :: Parser Token
peekToken = Parser $ \tokens -> case tokens of
    [] -> do
        code <- ask
        throwError $ ParseError "msg" code (Position 0 0)
    (t:_) -> return (token t, tokens)

-- basic combinators
satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = do
    t <- peekToken
    if predicate t
        then token <$> anyToken
        else throwParserError "Token does not satisfy predicate"

matchToken :: Token -> Parser Token
matchToken expected = satisfy (== expected)

skip :: Token -> Parser ()
skip expected = do
    t <- peekToken
    if t == expected
        then void anyToken
        else throwParserError $ "Expected token: " ++ show expected

punct :: String -> Parser ()
punct s = do
    tk <- satisfy isPunct
    case tk of
        TK_PUNCT p | p == s -> return ()
        _                   -> throwParserError $ "Expected punctuation: " ++ s
    where
        isPunct (TK_PUNCT _) = True
        isPunct _            = False

integer :: Parser Int
integer = do
    tk <- satisfy isNum
    case tk of
        TK_NUM n -> return n
        _        -> throwParserError "Expected a number"
    where
        isNum (TK_NUM _) = True
        isNum _          = False

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    x <- p
    rest x
    where rest x = do
            f <- op
            y <- p
            rest (f x y)
            <|> return x

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

program :: Parser Expr
program = do
    ast <- expr
    skip TK_EOF
    return ast

-- expr = equality
expr :: Parser Expr
expr = equality

-- equality = relational ("==" relational | "!=" relational)*
equality :: Parser Expr
equality = chainl1 relational (eqOp <|> neOp)
    where
        eqOp = punct "==" $> BinOp Eq
        neOp = punct "!=" $> BinOp Ne

-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*
relational :: Parser Expr
relational = chainl1 add (ltOp <|> leOp <|> gtOp <|> geOp)
    where
        ltOp = punct "<" $> BinOp Lt
        leOp = punct "<=" $> BinOp Le
        gtOp = punct ">" $> flip (BinOp Lt)
        geOp = punct ">=" $> flip (BinOp Le)

-- add = mul ("+" mul | "-" mul)*
add :: Parser Expr
add = chainl1 mul (addOp <|> subOp)
    where
        addOp = punct "+" $> BinOp Add
        subOp = punct "-" $> BinOp Sub

-- mul = unary ("*" unary | "/" unary)*
mul :: Parser Expr
mul = chainl1 unary (mulOp <|> divOp)
    where
        mulOp = punct "*" $> BinOp Mul
        divOp = punct "/" $> BinOp Div

-- unary = ("+" | "-") unary
--       | primary
unary :: Parser Expr
unary = (UnaryOp Neg <$> (punct "-" *> unary))
    <|> (UnaryOp Pos <$> (punct "+" *> unary))
    <|> primary

-- primary = "(" expr ")" | num
primary :: Parser Expr
primary = between (punct "(") (punct ")") expr
      <|> intLit
      <?> "expected an expression"

intLit :: Parser Expr
intLit = IntLit <$> integer

parse :: [PosToken] -> Code -> Either CompilerError Expr
parse tokens codes = do
    let parseResult = 
            runIdentity $ 
            runExceptT $ 
            flip runReaderT codes $ 
            runParser program tokens
    case parseResult of
        Right (exp, []) -> return exp
        Left e -> Left e

genExpr :: Expr -> Either CompilerError String
genExpr (IntLit n) = return $ "  mov $" ++ show n ++ ", %rax\n"
genExpr (BinOp op e1 e2) = do
    asm1 <- genExpr e1
    asm2 <- genExpr e2
    let opStr = case op of
            Add -> "  add %rdi, %rax\n"
            Sub -> "  sub %rdi, %rax\n"
            Mul -> "  imul %rdi, %rax\n"
            Div -> "  cqo\n"             ++
                   "  idiv %rdi\n"
            Eq ->  "  cmp %rdi, %rax\n"  ++
                   "  sete %al\n"        ++
                   "  movzb %al, %rax\n"
            Ne ->  "  cmp %rdi, %rax\n"  ++
                   "  setne %al\n"       ++
                   "  movzb %al, %rax\n"
            Lt ->  "  cmp %rdi, %rax\n"  ++
                   "  setl %al\n"        ++
                   "  movzb %al, %rax\n"
            Le ->  "  cmp %rdi, %rax\n"  ++
                   "  setle %al\n"       ++
                   "  movzb %al, %rax\n"
    return $
        asm2 ++
        push ++
        asm1 ++
        pop ++
        opStr
genExpr (UnaryOp Neg e) = do
    asm <- genExpr e
    return $ asm ++ "  neg %rax\n"
genExpr (UnaryOp Pos e) = genExpr e

push :: String
push = "  push %rax\n"

pop :: String
pop = "  pop %rdi\n"

main :: IO ()
main = getArgs >>= compile >>= either handleError putStr

handleError :: CompilerError -> IO ()
handleError (LexError msg expr pos) =
    hPutStr stderr $ formatError msg expr pos
handleError (ParseError msg expr pos) =
    hPutStr stderr $ formatError msg expr pos

formatError :: String -> Code -> Position -> String
formatError msg expr pos = unlines
    [ expr
    , replicate (startCol pos - 1) ' ' ++ "^ " ++ msg
    ]

compile :: [String] -> IO (Either CompilerError String)
compile []    = return $ Right ""
compile (codes:_) = return $ do
    tokens <- tokenize codes
    ast    <- parse tokens codes
    asm    <- genExpr ast
    return $
        "  .globl main\n" ++
        "main:\n" ++
        asm ++
        "  ret\n"
