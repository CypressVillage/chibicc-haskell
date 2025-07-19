import System.Environment ( getArgs )   

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse []    = return ""
parse [x]   = do
    putStrLn "  .globl main"
    putStrLn "main:"
    putStrLn $ "  mov $" ++ parseNum x ++ ", %rax"
    putStr   $ parseOperator x
    return   "  ret"

parseOperator :: String -> String
parseOperator []   = ""
parseOperator (x:xs)
    | x == '+'  = "  add $" ++ parseNum xs ++ ", %rax\n" ++ parseOperator xs
    | x == '-'  = "  sub $" ++ parseNum xs ++ ", %rax\n" ++ parseOperator xs
    | otherwise = parseOperator xs

parseNum :: String -> String
parseNum [] = ""
parseNum (x:xs)
    | x `elem` ['0'..'9'] = x : parseNum xs
    | otherwise           = ""