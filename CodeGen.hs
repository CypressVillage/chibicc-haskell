module CodeGen (genCode) where

import Data.Char ( ord )
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import CompilerData

type CodeEnv a = ExceptT CompilerError (StateT Int Identity) a

alignTo :: Int -> Int -> Int
alignTo align n = (n + align - 1) `div` align * align

prologue :: String
prologue = "  .globl main\n"    ++
           "main:\n"            ++
           "  push %rbp\n"      ++
           "  mov %rsp, %rbp\n"

epilogue :: String
epilogue = ".L.return:\n"       ++
           "  mov %rbp, %rsp\n" ++ -- Restore stack pointer
           "  pop %rbp\n"       ++
           "  ret\n"

push :: String
push = "  push %rax\n"

pop :: String
pop = "  pop %rdi\n"

stackInit :: Int -> String
stackInit size = "  sub $" ++ show size ++ ", %rsp\n"

genOp :: BinOp -> String
genOp Add = "  add %rdi, %rax\n"
genOp Sub = "  sub %rdi, %rax\n"
genOp Mul = "  imul %rdi, %rax\n"
genOp Div = "  cqo\n"             ++
            "  idiv %rdi\n"
genOp Eq  = "  cmp %rdi, %rax\n"  ++
            "  sete %al\n"        ++
            "  movzb %al, %rax\n"
genOp Ne  = "  cmp %rdi, %rax\n"  ++
            "  setne %al\n"       ++
            "  movzb %al, %rax\n"
genOp Lt  = "  cmp %rdi, %rax\n"  ++
            "  setl %al\n"        ++
            "  movzb %al, %rax\n"
genOp Le  = "  cmp %rdi, %rax\n"  ++
            "  setle %al\n"       ++
            "  movzb %al, %rax\n"

genExpr :: Expr -> CodeEnv String
genExpr (IntLit n) = return $ "  mov $" ++ show n ++ ", %rax\n"
genExpr (BinOp op e1 e2) = do
    asm1 <- genExpr e1
    asm2 <- genExpr e2
    let opStr = genOp op
    return $ asm2 ++ push ++ asm1 ++ pop ++ opStr
genExpr (UnaryOp Neg e) = do
    asm <- genExpr e
    return $ asm ++ "  neg %rax\n"
genExpr (UnaryOp Pos e) = genExpr e
genExpr (Var var) = do
    addr <- genAddr var
    return $ addr ++ "  mov (%rax), %rax\n"
genExpr (Assign var e) = do
    asm <- genExpr e
    addr <- genAddr var
    return $ addr ++ push ++ asm ++ pop ++ "  mov %rax, (%rdi)\n"

genStmt :: Stmt -> CodeEnv String
genStmt (ExprStmt expr) = genExpr expr
genStmt (ReturnStmt expr) = do
    asm <- genExpr expr
    return $ asm ++ "  jmp .L.return\n"
genStmt (CompoundStmt stmts) = mconcat <$> mapM genStmt stmts
genStmt (IfStmt cond thn mayEls) = do
    i <- get
    put $ i + 1
    condCode <- genExpr cond
    thenCode <- genStmt thn
    elseCode <- maybe (return "") genStmt mayEls
    return $ condCode                           ++
             "  cmp $0, %rax\n"                 ++
             "  je  .L.else." ++ show i ++ "\n" ++
             thenCode                           ++
             "  jmp .L.end." ++ show i ++ "\n"  ++
             ".L.else." ++ show i ++ ":\n"      ++
             elseCode                           ++
             ".L.end." ++ show i ++ ":\n"
genStmt (ForStmt init mayCond mayInc thn) = do
    i <- get
    put $ i + 1
    initCode <- genStmt init
    condCode <- maybe (return "") genExpr mayCond
    incCode  <- maybe (return "") genExpr mayInc
    thenCode <- genStmt thn
    return $ initCode ++ 
             ".L.begin." ++ show i ++ ":\n"      ++
             condCode                            ++
             (if null condCode then "" else
             "  cmp $0, %rax\n"                  ++
             "  je  .L.end." ++ show i ++"\n")   ++
             thenCode                            ++
             incCode                             ++
             "  jmp .L.begin." ++ show i ++ "\n" ++
             ".L.end." ++ show i ++ ":\n"


genAddr :: LocalVal -> CodeEnv String
genAddr (LocalVal _ off) = return $ "  lea " ++ show (-off) ++ "(%rbp), %rax\n"

genFunction :: Function -> CodeEnv String
genFunction func = do
    let Function ast locals stackSize = func
    let size = alignTo 16 stackSize
    asms <- mapM genStmt ast
    return $ prologue ++ stackInit size ++ concat asms ++ epilogue

genCode :: Function -> Either CompilerError String
genCode func = fst $ runIdentity $ flip runStateT 1 $ runExceptT $ genFunction func