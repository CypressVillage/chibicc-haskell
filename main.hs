import System.Environment ( getArgs )

import Types ( CompilerError )
import Errorhandle ( handleError )
import Tokenize ( tokenize )
import Parse ( parse )
import Codegen ( codeGen )

main :: IO ()
main = getArgs >>= compile >>= either handleError putStr

compile :: [String] -> IO (Either CompilerError String)
compile []    = return $ Right ""
compile (codes:_) = pure $ do
    tokens <- tokenize codes
    ast    <- parse codes tokens
    codeGen $ fst ast
