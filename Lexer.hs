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
            let pos = Position col (col+len-1)
            in (PosToken (TK_NUM (read num)) pos : ) <$> tokenize' (col+len) nnum
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
              len             = length num