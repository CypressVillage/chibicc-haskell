module Parse ( parse ) where

import Types

parseLeftAssoc
    :: (Code -> [Token] -> Either CompilerError (Node, [Token])) -- 子表达式解析器
    -> [(String, Node -> Node -> Node)]                          -- 操作符列表
    -> Code -> [Token] -> Either CompilerError (Node, [Token])
parseLeftAssoc parseOp ops codes tokens = do
    (left, tokens') <- parseOp codes tokens
    parseLeftAssoc' left tokens'
    where
        parseLeftAssoc' left [] = Right (left, [])
        parseLeftAssoc' left (tk:tks) = case tokenKind tk of
            TK_PUNCT op -> case lookup op ops of
                Just opFunc -> do
                    (right, tks') <- parseOp codes tks
                    parseLeftAssoc' (opFunc left right) tks'
                Nothing -> Right (left, tk:tks)
            _ -> Right (left, tk:tks)

-- program = stmt*
parse :: Code -> [Token] -> Either CompilerError (Node, [Token])
parse = parseStmt

-- stmt = expr-stmt
parseStmt :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseStmt = parseExprStmt

-- expr-stmt = expr ";"
parseExprStmt :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseExprStmt codes [] = Right (ND_EMPTY, [])
parseExprStmt codes tokens = do
    (left, rest) <- parseExpr codes tokens
    case rest of
        [] -> Left $ ParseError "expected ';' at the end of expression statement" codes (position $ last tokens)
        [Token TK_EOF _] -> Right (ND_EXPR_STMT left ND_EMPTY, [])
        (tk:rest') -> case tokenKind tk of
            TK_PUNCT ";" -> do
                (right, rest'') <- parseExprStmt codes rest'
                Right (ND_EXPR_STMT left right, rest'')
            _            -> Left $ ParseError "expected ';' at the end of expression statement" codes (position tk)

-- expr = assign
parseExpr :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseExpr = parseAssign

-- assign = equality ("=" assign)?
parseAssign :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseAssign codes tokens = do
    (left, rest) <- parseEquality codes tokens
    case rest of
        [] -> Right (left, [])
        (Token (TK_PUNCT "=") pos:rest') -> do
            (right, rest'') <- parseAssign codes rest'
            case left of
                ND_VAR varName -> Right (ND_ASSIGN varName right, rest'')
                _              -> Left $ ParseError "left-hand side of assignment must be a variable" codes pos
        _ -> Right (left, rest)

-- equality = relational ("==" relational | "!=" relational)*
parseEquality :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseEquality = parseLeftAssoc parseRelational
    [ ("==", ND_OP ND_EQ)
    , ("!=", ND_OP ND_NE)
    ]

-- relational = add ("<" add | "<=" add | ">" add | ">=" add)*
parseRelational :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseRelational = parseLeftAssoc parseAdd
    [ ("<", ND_OP ND_LT)
    , ("<=", ND_OP ND_LE)
    , (">", flip $ ND_OP ND_LT)
    , (">=", flip $ ND_OP ND_LE)
    ]

-- add = mul ("+" mul | "-" mul)*
parseAdd :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseAdd = parseLeftAssoc parseMul
    [ ("+", ND_OP ND_ADD)
    , ("-", ND_OP ND_SUB)
    ]

-- mul = unary ("*" unary | "/" unary)*
parseMul :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseMul = parseLeftAssoc parseUnary
    [ ("*", ND_OP ND_MUL)
    , ("/", ND_OP ND_DIV)
    ]

-- unary = ("+" | "-") unary
--       | primary
parseUnary :: Code -> [Token] -> Either CompilerError (Node, [Token])
parseUnary codes (tk:rest) = case tokenKind tk of
    TK_PUNCT "+" -> parseUnary codes rest
    TK_PUNCT "-" -> do
        (right, tks) <- parseUnary codes rest
        Right (ND_NEG right, tks)
    _ -> parsePrimary codes (tk:rest)

-- primary = "(" expr ")" | num | ident
parsePrimary :: Code -> [Token] -> Either CompilerError (Node, [Token])
parsePrimary codes (tk:rest) = case tokenKind tk of
    TK_PUNCT "(" -> case parseExpr codes rest of
        Left e -> Left e
        Right (node, rParen:rest) -> case tokenKind rParen of
            TK_PUNCT ")" -> Right (node, rest)
            _            -> Left $ ParseError "expected ')'" codes (position rParen)
        Right (node, []) -> Left $ ParseError "expected ')'" codes (position $ last rest)

    TK_NUM n ->
        Right (ND_NUM n, rest)

    TK_IDENT ident ->
        Right (ND_VAR ident, rest)

    other -> Right (ND_EMPTY, tk:rest)
