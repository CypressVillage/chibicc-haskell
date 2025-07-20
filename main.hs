import System.Environment ( getArgs )
import Data.Char ( isDigit )

data Token = TK_PUNCT String
           | TK_NUM Int
           | TK_EOF
           deriving (Show)

main :: IO ()
main = getArgs >>= parse >>= putStr

parse :: [String] -> IO String
parse []    = return ""
parse [x]   = do
    putStrLn "  .globl main"
    putStrLn "main:"
    return $ (parseToken . tokenize) x

parseOperator :: Token -> Token -> String
parseOperator (TK_PUNCT x) (TK_NUM n)
    | x == "+"  = "  add $" ++ num ++ ", %rax\n"
    | x == "-"  = "  sub $" ++ num ++ ", %rax\n"
    where num = show n

parseNum :: Token -> String
parseNum (TK_NUM n) = "  mov $" ++ num ++ ", %rax\n"
    where num = show n

tokenize :: String -> [Token]
tokenize []                 = [TK_EOF]
tokenize s@(x:xs)
    | x == ' '              = tokenize xs
    | x `elem` punctuators  = TK_PUNCT [x] : tokenize xs
    | isDigit x             = TK_NUM (read num) : tokenize other
    where punctuators = "+-"
          (num, other) = span isDigit s

parseToken :: [Token] -> String
parseToken []       = ""
parseToken s@(x:xs) =
    case x of TK_PUNCT _ -> parseOperator x (head xs) ++ parseToken (tail xs)
              TK_NUM _   -> parseNum x ++ parseToken xs
              TK_EOF     -> "  ret\n"