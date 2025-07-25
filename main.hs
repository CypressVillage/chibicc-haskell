import System.Environment ( getArgs )

import Types ( CompilerError )
import Errorhandle ( handleError )
import Tokenize ( tokenize )
import Parse ( parseExpr )
import Codegen ( genExpr )

main :: IO ()
main = getArgs >>= compile >>= either handleError putStr

compile :: [String] -> IO (Either CompilerError String)
compile []    = return $ Right ""
compile (codes:_) = pure $ do
    tokens <- tokenize codes
    ast    <- parseExpr codes tokens
    asm    <- genExpr $ fst ast
    return $ unlines
        [ "  .globl main"
        , "main:"
        , asm
        , "  ret"
        ]
