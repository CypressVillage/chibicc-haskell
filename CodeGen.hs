module CodeGen (genCode) where

import Data.Char ( ord )
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import CompilerData
import Data.List (findIndex)

type CodeEnv a = ReaderT (String, [LocalVal]) (ExceptT CompilerError (StateT Int Identity)) a

alignTo :: Int -> Int -> Int
alignTo align n = (n + align - 1) `div` align * align

prologue :: CodeEnv String
prologue = do
    (fnName, locals) <- ask
    let stkSize = show $ alignTo 16 (8 * length locals)
    return $ "  .globl " ++ fnName ++ "\n"    ++
           fnName ++ ":\n"                    ++
           "  push %rbp\n"                    ++
           "  mov %rsp, %rbp\n"               ++
           "  sub $" ++ stkSize ++ ", %rsp\n"    -- Allocate stack space

epilogue :: CodeEnv String
epilogue = do
    fnName <- asks fst
    return $ ".L.return." ++ fnName ++ ":\n" ++
           "  mov %rbp, %rsp\n"              ++ -- Restore stack pointer
           pop "rbp"                         ++
           "  ret\n"

push :: String
push = "  push %rax\n"

pop :: String -> String
pop reg = "  pop %" ++ reg ++ "\n"

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
genExpr (IntLit n _) = return $ "  mov $" ++ show n ++ ", %rax\n"
genExpr (BinOp op _ e1 e2) = do
    asm1 <- genExpr e1
    asm2 <- genExpr e2
    let opStr = genOp op
    return $ asm2 ++ push ++ asm1 ++ pop "rdi" ++ opStr
genExpr (UnaryOp Neg _ e) = do
    asm <- genExpr e
    return $ asm ++ "  neg %rax\n"
genExpr (UnaryOp Pos _ e) = genExpr e
genExpr (UnaryOp Addr _ var) = genAddr var
genExpr (UnaryOp DeRef _ ref) = do
    val <- genExpr ref
    return $ val ++ "  mov (%rax), %rax\n"
genExpr (Var var t) = do
    addr <- genAddr $ Var var t
    return $ addr ++ "  mov (%rax), %rax\n"
genExpr (Assign _ var e) = do
    asm <- genExpr e
    addr <- genAddr var
    return $ addr ++ push ++ asm ++ pop "rdi" ++ "  mov %rax, (%rdi)\n"
genExpr (FunCall _ name args) = do
    argAsms <- mapM genExpr args
    let pushArgs = concatMap (++ push) argAsms
    let popArgs = concatMap (pop . regName) (reverse [0..length args - 1])
    return $ pushArgs ++ popArgs ++
            "  mov $0, %rax\n" ++
            "  call " ++ name ++ "\n"
  where
    regName 0 = "rdi"
    regName 1 = "rsi"
    regName 2 = "rdx"
    regName 3 = "rcx"
    regName 4 = "r8"
    regName 5 = "r9"
    regName _ = error "Too many function arguments"

genStmt :: Stmt -> CodeEnv String
genStmt (ExprStmt expr) = genExpr expr
genStmt (ReturnStmt expr) = do
    fnName <- asks fst
    asm <- genExpr expr
    return $ asm ++ "  jmp .L.return." ++ fnName ++ "\n"
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
    initCode <- maybe (return "") genStmt init
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


genAddr :: Expr -> CodeEnv String
genAddr (Var (LocalVal name' _) _) = do
    localVals <- asks snd
    let off = case findIndex (\v -> name v == name') localVals of
            Just i -> offset (localVals !! i)
    return $ "  lea " ++ show (-off) ++ "(%rbp), %rax\n"
genAddr (UnaryOp DeRef _ d) = genExpr d

genFunction :: Function -> CodeEnv String
genFunction (Function rtnT fnName fnBody locals) = do
    localVals <- asks snd
    let newLocalVals = localVals ++ locals
    asms <- local (const (fnName, newLocalVals)) $ mconcat <$> mapM genStmt fnBody
    prologueCode <- local (const (fnName, locals)) prologue
    epilogueCode <- local (const (fnName, locals)) epilogue
    return $ prologueCode ++ asms ++ epilogueCode

genCFile :: CFile -> CodeEnv String
genCFile (CFile funcs) = do
    funcAsms <- mapM genFunction funcs
    return $ concat funcAsms

genCode :: CFile -> Either CompilerError String
genCode cfile = fst
    $ runIdentity
    $ flip runStateT 1
    $ runExceptT
    $ flip runReaderT ("", [])
    $ genCFile cfile