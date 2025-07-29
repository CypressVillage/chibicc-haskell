module Tokenize where

import Data.Char ( isDigit )
import Types
import Data.ByteString (split)

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
            let pos = Position col (col+length num-1)
            in (Token (TK_NUM (read num)) pos : ) <$> tokenize' (col+length num) nnum
        | isIdent x           =
            let (ident, nident) = span isIdent s
                pos = Position col (col + length ident - 1)
            in (Token (TK_IDENT ident) pos : ) <$> tokenize' (col + length ident) nident
        | otherwise             =
            let pos = Position col col
            in Left $ LexError "invalid token" input pos
        where isPunct         = (`elem` "+-*/()<>=!;")
              (punct, npunct) = splitPunct s
              splitPunct ('=':'=':n) = ("==", n)
              splitPunct ('!':'=':n) = ("!=", n)
              splitPunct ('>':'=':n) = (">=", n)
              splitPunct ('<':'=':n) = ("<=", n)
              splitPunct (punct  :n) = ([punct], n)
              (num, nnum)     = span isDigit s
              isIdent         = (`elem` ['a'..'z'])