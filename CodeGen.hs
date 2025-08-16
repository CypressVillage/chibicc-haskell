module CodeGen (genCode) where

import CompilerData
import Data.Char ( ord )

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

genExpr :: Expr -> Either CompilerError String
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

genStmt :: Stmt -> Either CompilerError String
genStmt (ExprStmt expr) = genExpr expr
genStmt (ReturnStmt expr) = do
    asm <- genExpr expr
    return $ asm ++ "  jmp .L.return\n"
genStmt (CompoundStmt stmts) = mconcat <$> mapM genStmt stmts

genAddr :: LocalVal -> Either CompilerError String
genAddr (LocalVal _ off) = Right $ "  lea " ++ show (-off) ++ "(%rbp), %rax\n"

genCode :: Function -> Either CompilerError String
genCode func = do
    let Function ast locals stackSize = func
    let size = alignTo 16 stackSize
    asms <- mapM genStmt ast
    return $ prologue ++ stackInit size ++ concat asms ++ epilogue
