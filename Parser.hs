{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser (parse) where

import Data.List (findIndex)
import Data.Functor (($>))
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative (Alternative (many), empty, (<|>))
import CompilerData

type ParserEnv a = ReaderT Code (ExceptT CompilerError (StateT [LocalVal] Identity)) a
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

instance MonadState [LocalVal] Parser where
    get :: Parser [LocalVal]
    get = Parser $ \tokens -> do
        localVals <- get
        return (localVals, tokens)

    put :: [LocalVal] -> Parser ()
    put localVals = Parser $ \tokens -> do
        put localVals
        return ((), tokens)

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
punct s = satisfy (== TK_PUNCT s) $> ()

eof :: Parser ()
eof = skip TK_EOF

integer :: Parser Int
integer = do
    tk <- satisfy isNum
    case tk of
        TK_NUM n -> return n
        _        -> throwParserError "Expected a number"
    where
        isNum (TK_NUM _) = True
        isNum _          = False

identity :: Parser String
identity = do
    tk <- satisfy isIdent
    case tk of
        TK_IDENT name -> return name
        _             -> throwParserError "Expected an identifier"
    where
        isIdent (TK_IDENT _) = True
        isIdent _            = False

keyword :: String -> Parser ()
keyword s = satisfy (== TK_KEYWORD s) $> ()

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> return Nothing

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    x <- p
    rest x
    where rest x = do
            f <- op
            y <- p
            rest (f x y)
            <|> return x

-- program = stmt* eof
program :: Parser [Stmt]
program = many stmt <* eof

-- stmt = "return" expr ";"
--      | exprStmt
stmt :: Parser Stmt
stmt = ReturnStmt <$> (keyword "return" *> expr <* punct ";")
   <|> exprStmt

-- exprStmt = expr ";"
exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr <* punct ";"

-- expr = assign
expr :: Parser Expr
expr = assign

-- assign = equality ("=" assign)?
assign :: Parser Expr
assign = do
    e <- equality
    rest <- optional (punct "=" *> assign)
    case e of
        Var varName -> case rest of
            Just e' -> return $ Assign varName e'
            Nothing -> return e
        _ -> case rest of
            Just e' -> throwParserError "Left-hand side of assignment must be a variable"
            Nothing -> return e

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

-- primary = "(" expr ")" | num | ident
primary :: Parser Expr
primary = between (punct "(") (punct ")") expr
      <|> intLit
      <|> ident
      <?> "expected an expression"

intLit :: Parser Expr
intLit = IntLit <$> integer

ident :: Parser Expr
ident = do
    name' <- identity
    localVals <- get
    case findIndex (\v -> name v == name') localVals of
        Just idx -> return $ Var $ localVals !! idx
        Nothing  -> do
            let offset' = 8 + sum (map offset localVals)
            let val = LocalVal name' offset'
            put (val : localVals)
            return $ Var val

parse :: Code -> [PosToken] -> Either CompilerError Function
parse codes tokens = do
    let parseResult = 
            runIdentity $ 
            flip runStateT [] $
            runExceptT $ 
            flip runReaderT codes $ 
            runParser program tokens
    case parseResult of
        (Right (ast, []), localVals) -> return Function
            { body = ast
            , locals = localVals
            , stackSize = if null localVals then 0 else offset $ head localVals
            }
        (Left e, _) -> Left e
