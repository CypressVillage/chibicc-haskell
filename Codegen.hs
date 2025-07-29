module Codegen where

import Types

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

genStmt :: Node -> Either CompilerError String
genStmt ND_EMPTY = Right ""
genStmt (ND_EXPR_STMT expr expr') = do
    exprCode <- genExpr expr
    restCode <- genStmt expr'
    return $ exprCode ++ restCode

codeGen :: Node -> Either CompilerError String
codeGen node = do
    let init = "  .globl main\n" ++
               "main:\n"
    stmtCode <- genStmt node
    let last = "  ret\n"
    return $ concat
        [ init
        , stmtCode
        , last
        ]