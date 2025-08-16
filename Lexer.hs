module Lexer (tokenize) where

import Data.Char ( isDigit )
import CompilerData

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
            let pos = Position col (col+length num-1)
            in (PosToken (TK_NUM (read num)) pos : ) <$> tokenize' (col+length num) nnum
        | isIdent1 x            =
            let (ident, nident) = span isIdent2 s
                pos = Position col (col + length ident - 1)
            in case ident of
                "return" ->
                    (PosToken (TK_KEYWORD "return") pos : ) <$> tokenize' (col + length ident) nident
                _ -> 
                    (PosToken (TK_IDENT ident) pos : ) <$> tokenize' (col + length ident) nident
        | otherwise              =
            let pos = Position col col
            in Left $ LexError "invalid token" input pos
        where isPunct         = (`elem` "+-*/()<>=!;")
              (punct, npunct) = splitPunct s
              splitPunct ('=':'=':n) = ("==", n)
              splitPunct ('!':'=':n) = ("!=", n)
              splitPunct ('>':'=':n) = (">=", n)
              splitPunct ('<':'=':n) = ("<=", n)
              splitPunct (punct  :n) = ([punct], n)
              (num, nnum)      = span isDigit s
              isIdent1         = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
              isIdent2 c       = isIdent1 c || isDigit c