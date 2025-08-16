module Error where

import System.IO ( hPutStr, stderr )
import CompilerData

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