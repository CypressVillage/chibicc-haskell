module Tokenize where

import Data.Char ( isDigit )
import Types

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