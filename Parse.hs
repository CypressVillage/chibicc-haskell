module Parse ( parseExpr ) where

import Types

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

