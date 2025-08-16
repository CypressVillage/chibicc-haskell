module CodeGen (genCode) where

import CompilerData

prologue :: String
prologue = "  .globl main\n" ++
           "main:\n"

epilogue :: String
epilogue = "  ret\n"

push :: String
push = "  push %rax\n"

pop :: String
pop = "  pop %rdi\n"

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

genCode :: Expr -> Either CompilerError String
genCode expr = do
    asm <- genExpr expr
    return $ prologue ++ asm ++ epilogue

