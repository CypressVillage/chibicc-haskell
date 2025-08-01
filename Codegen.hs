module Codegen where

import Types
import Data.Char ( ord )

genExpr :: Node -> Either CompilerError String
genExpr ND_EMPTY         = Right ""
genExpr (ND_NUM n)        = Right $ "  mov $" ++ show n ++ ", %rax\n"
genExpr (ND_NEG expr)     = (++ "  neg %rax\n") <$> genExpr expr
genExpr (ND_OP ndType l r) = do
    rCode <- genExpr r
    lCode <- genExpr l
    let opCode = case ndType of
            ND_ADD -> "  add %rdi, %rax\n"
            ND_SUB -> "  sub %rdi, %rax\n"
            ND_MUL -> "  imul %rdi, %rax\n"
            ND_DIV -> "  cqo\n"             ++
                      "  idiv %rdi\n"
            ND_EQ  -> "  cmp %rdi, %rax\n"  ++
                      "  sete %al\n"        ++
                      "  movzb %al, %rax\n"
            ND_NE  -> "  cmp %rdi, %rax\n"  ++
                      "  setne %al\n"       ++
                      "  movzb %al, %rax\n"
            ND_LT  -> "  cmp %rdi, %rax\n"  ++
                      "  setl %al\n"        ++
                      "  movzb %al, %rax\n"
            ND_LE  -> "  cmp %rdi, %rax\n"  ++
                      "  setle %al\n"       ++
                      "  movzb %al, %rax\n"
    Right $ rCode ++
            "  push %rax\n" ++
            lCode ++
            "  pop %rdi\n" ++
            opCode
genExpr (ND_VAR ident) = do
    addrCode <- genAddr (ND_VAR ident)
    Right $ addrCode ++ "  mov (%rax), %rax\n"
genExpr (ND_ASSIGN ident expr) = do
    addrCode <- genAddr (ND_VAR ident)
    let push = "  push %rax\n"
    exprCode <- genExpr expr
    let pop = "  pop %rdi\n"
    Right $ addrCode ++ push ++ exprCode ++ pop ++ "  mov %rax, (%rdi)\n"

genStmt :: Node -> Either CompilerError String
genStmt ND_EMPTY = Right ""
genStmt (ND_EXPR_STMT expr expr') = do
    exprCode <- genExpr expr
    restCode <- genStmt expr'
    return $ exprCode ++ restCode

genAddr :: Node -> Either CompilerError String
genAddr (ND_VAR ident) = Right $ "  lea " ++ show (ident' ident) ++ "(%rbp), %rax\n"
    where ident' (x:_) = - ((ord x - ord 'a' + 1) * 8)
          ident' _     = 0
genAddr _ = Left $ LexError "expected variable" "" (Position 0 0)

codeGen :: Node -> Either CompilerError String
codeGen node = do
    let init = "  .globl main\n" ++
               "main:\n"
    let prologue = "  push %rbp\n" ++
                   "  mov %rsp, %rbp\n"
    stmtCode <- genStmt node
    let epilogue = "  mov %rbp, %rsp\n" ++ -- Restore stack pointer
                   "  pop %rbp\n"
    let last = "  ret\n"
    return $ concat
        [ init
        , prologue
        , "  sub $208, %rsp\n"  -- Allocate space for local variables
        , stmtCode
        , epilogue
        , last
        ]