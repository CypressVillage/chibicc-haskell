import System.Environment ( getArgs )   

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse []    = return ""
parse [x]   = do
    putStrLn "  .globl main"
    putStrLn "main:"
    putStrLn $ "  mov $" ++ x ++ ", %rax"
    return   "  ret"