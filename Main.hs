import System.Environment (getArgs)
import CompilerData ( CompilerError )
import Error (handleError)
import Lexer (tokenize)
import Parser (parse)
import Semantic (semantic)
import CodeGen (genCode)

main :: IO ()
main = getArgs >>= compile >>= either handleError putStr

compile :: [String] -> IO (Either CompilerError String)
compile (codes:_) = return
    $ tokenize codes
    >>= parse codes
    >>= semantic
    >>= genCode