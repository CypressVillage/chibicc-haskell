module Parse ( parse, runParser ) where

import Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity (Identity (runIdentity))
import Data.List ( findIndex )

type ParserEnv a = ReaderT Code (ExceptT CompilerError (StateT [LocalVal] Identity)) a
type Parser a = [Token] -> ParserEnv (a, [Token])

runParser :: Parser Node -> [Token]
    -> Code
    -> Either CompilerError Function
runParser p tokens code = do
    let (parserRtn, localVals) = runIdentity
            $ flip runStateT []
            $ runExceptT
            $ flip runReaderT code
            $ p tokens
    case parserRtn of
        Left err -> Left err
        Right result -> do
            let size = case localVals of
                    [] -> 0
                    _ -> offset $ head localVals
            return $ Function (fst result) localVals size

parseLeftAssoc
    :: Parser Node -- 子表达式解析器
    -> [(String, Node -> Node -> Node)] -- 操作符列表
    -> Parser Node
parseLeftAssoc parseOp ops tokens = do
    (left, tokens') <- parseOp tokens
    parseLeftAssoc' left tokens'
    where
        parseLeftAssoc' left [] = return (left, [])
        parseLeftAssoc' left (tk:tks) = case tokenKind tk of
            TK_PUNCT op -> case lookup op ops of
                Just opFunc -> do
                    (right, tks') <- parseOp tks
                    parseLeftAssoc' (opFunc left right) tks'
                Nothing -> return (left, tk:tks)
            _ -> return (left, tk:tks)

-- program = stmt*
parse :: Parser Node
parse = parseStmt

-- stmt = "return" expr ";"
--      | expr-stmt
parseStmt :: Parser Node
parseStmt (tk:tks) = case tokenKind tk of
    TK_KEYWORD "return" -> do
        codes <- ask
        (left, rest) <- parseExpr tks
        case rest of
            [] -> throwError $ ParseError "expected ';' at the end of expression statement" codes (position $ last tks)
            (tk':rest') -> case tokenKind tk' of
                TK_PUNCT ";" -> do
                    (right, rest'') <- parseStmt rest'
                    return (ND_RETURN left right, rest'')

    _ -> parseExprStmt (tk:tks)

-- expr-stmt = expr ";"
parseExprStmt :: Parser Node
parseExprStmt [] = return (ND_EMPTY, [])
parseExprStmt tokens = do
    codes <- ask
    (left, rest) <- parseExpr tokens
    case rest of
        [] -> throwError $ ParseError "expected ';' at the end of expression statement" codes (position $ last tokens)
        [Token TK_EOF _] -> return (ND_EXPR_STMT left ND_EMPTY, [])
        (tk:rest') -> case tokenKind tk of
            TK_PUNCT ";" -> do
                (right, rest'') <- parseStmt rest' -- when current stmt ok, gen next stmt
                return (ND_EXPR_STMT left right, rest'')
            _            -> throwError $ ParseError "expected ';' at the end of expression statement" codes (position tk)

-- expr = assign
parseExpr :: Parser Node
parseExpr = parseAssign

-- assign = equality ("=" assign)?
parseAssign :: Parser Node
parseAssign tokens = do
    codes <- ask
    (left, rest) <- parseEquality tokens
    case rest of
        [] -> return (left, [])
        (Token (TK_PUNCT "=") pos:rest') -> do
            (right, rest'') <- parseAssign rest'
            case left of
                ND_VAR var -> return (ND_ASSIGN var right, rest'')
                _              -> throwError $ ParseError "left-hand side of assignment must be a variable" codes pos
        _ -> return (left, rest)

-- equality = relational ("==" relational | "!=" relational)*
parseEquality :: Parser Node
parseEquality = parseLeftAssoc parseRelational
    [ ("==", ND_OP ND_EQ)
    , ("!=", ND_OP ND_NE)
    ]

-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*
parseRelational :: Parser Node
parseRelational = parseLeftAssoc parseAdd
    [ ("<", ND_OP ND_LT)
    , ("<=", ND_OP ND_LE)
    , (">", flip $ ND_OP ND_LT)
    , (">=", flip $ ND_OP ND_LE)
    ]

-- add = mul ("+" mul | "-" mul)*
parseAdd :: Parser Node
parseAdd = parseLeftAssoc parseMul
    [ ("+", ND_OP ND_ADD)
    , ("-", ND_OP ND_SUB)
    ]

-- mul = unary ("*" unary | "/" unary)*
parseMul :: Parser Node
parseMul = parseLeftAssoc parseUnary
    [ ("*", ND_OP ND_MUL)
    , ("/", ND_OP ND_DIV)
    ]

-- unary = ("+" | "-") unary
--       | primary
parseUnary :: Parser Node
parseUnary (tk:rest) = case tokenKind tk of
    TK_PUNCT "+" -> parseUnary rest
    TK_PUNCT "-" -> do
        (right, tks) <- parseUnary rest
        return (ND_NEG right, tks)
    _ -> parsePrimary (tk:rest)

-- primary = "(" expr ")" | num | ident
parsePrimary :: Parser Node
parsePrimary (tk:rest) = do
    codes <- ask
    case tokenKind tk of
        TK_PUNCT "(" -> do
            (node, tokens') <- parseExpr rest
            case tokens' of
                (rParen:rest') ->
                    case tokenKind rParen of
                        TK_PUNCT ")" -> return (node, rest')
                        _            -> throwError $ ParseError "expected ')'" codes (position rParen)
                [] -> throwError $ ParseError "expected ')'" codes (position $ last rest)
        TK_NUM n ->
            return (ND_NUM n, rest)
        TK_IDENT ident -> do
            localVals <- get
            case findIndex (\v -> name v == ident) localVals of
                Just idx ->
                    return (ND_VAR (localVals !! idx), rest)
                Nothing -> do
                    let offset' = 8 + sum (map offset localVals)
                    let val = LocalVal ident offset'
                    put (val : localVals)
                    return (ND_VAR val, rest)
        _ ->
            return (ND_EMPTY, tk:rest)
