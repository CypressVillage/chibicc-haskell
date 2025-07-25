module Errorhandle where

import System.IO ( hPutStr, stderr )
import Types

handleError :: CompilerError -> IO ()
handleError (LexError msg code pos) =
    hPutStr stderr $ formatError msg code pos
handleError (ParseError msg code pos) =
    hPutStr stderr $ formatError msg code pos

formatError :: [Char] -> String -> Position -> String
formatError msg expr pos = unlines
    [ expr
    , replicate (startCol pos - 1) ' ' ++ "^ " ++ msg
    ]