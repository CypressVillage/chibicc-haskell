import System.IO ( hPutStr, stderr )
import System.Environment ( getArgs )
import Data.Char ( isDigit )

data Token = TK_PUNCT String
           | TK_NUM Int
           | TK_EOF
           deriving (Show)

data Position = Position
    { startCol :: Int
    , endCol   :: Int
    } deriving (Show)

data LocatedToken = LocatedToken
    { token    :: Token
    , position :: Position
    } deriving (Show)

data CompilerError = LexError String String Position
                   | ParseError String String Position
                   deriving (Show)

main :: IO ()
main = getArgs >>= parse >>= either handleError putStr
    where
        handleError (LexError msg expr pos) =
            hPutStr stderr $ formatError msg expr pos
        handleError (ParseError msg expr pos) =
            hPutStr stderr $ formatError msg expr pos

        formatError msg expr pos = unlines
            [ expr
            , replicate (startCol pos - 1) ' ' ++ "^ " ++ msg
            ]

parse :: [String] -> IO (Either CompilerError String)
parse []    = return $ Right ""
parse (x:_) = pure $ do
    tokens <- tokenize x
    asm    <- parseToken x tokens
    return $ unlines
        [ "  .globl main"
        , "main:"
        , asm
        , "  ret"
        ]

parseOperator :: String -> Int -> String
parseOperator p n
    | p == "+"  = "  add $" ++ num ++ ", %rax\n"
    | p == "-"  = "  sub $" ++ num ++ ", %rax\n"
    where num = show n

parseNum :: Int -> String
parseNum n = "  mov $" ++ show n ++ ", %rax\n"

tokenize :: String -> Either CompilerError [LocatedToken]
tokenize input = tokenize' 1 input where
    tokenize' :: Int -> String -> Either CompilerError [LocatedToken]
    tokenize' col []             = Right [LocatedToken TK_EOF (Position col col)]
    tokenize' col s@(x:xs)
        | x == ' '              = tokenize' (col+1) xs
        | x `elem` punctuators  =
            let pos = Position col col
            in (LocatedToken (TK_PUNCT [x]) pos : ) <$> tokenize' (col+1) xs
        | isDigit x             =
            let pos = Position col (col+len-1)
            in (LocatedToken (TK_NUM (read num)) pos : ) <$> tokenize' (col+len) other
        | otherwise             =
            let pos = Position col col
            in Left $ LexError "invalid token" input pos
        where punctuators  = "+-"
              (num, other) = span isDigit s
              len          = length num

parseToken :: String -> [LocatedToken] -> Either CompilerError String
parseToken expr []                           = Right ""
parseToken expr (LocatedToken (TK_PUNCT p) _   : LocatedToken (TK_NUM n) _   : xs) =
    (parseOperator p n ++) <$> parseToken expr xs
parseToken expr (LocatedToken (TK_PUNCT _) _   : LocatedToken  _         pos : xs) =
    Left $ ParseError "expected a number" expr pos
parseToken expr (LocatedToken (TK_NUM   n) _   : xs                              ) =
    (parseNum n ++) <$> parseToken expr xs
parseToken expr (LocatedToken  TK_EOF      _   : xs                              ) =
    Right ""