module Semantic where

import CompilerData

cType :: Expr -> CType
cType (IntLit _ t)    = t
cType (BinOp _ t _ _) = t
cType (UnaryOp _ t _) = t
cType (Var _ t)       = t
cType (Assign t _ _)  = t

mapExpr :: Monad m => (Expr_ a -> m (Expr_ a)) -> Expr_ a -> m (Expr_ a)
mapExpr f e@(IntLit _ _) = f e
mapExpr f (BinOp op t e1 e2) = do
    e1' <- mapExpr f e1
    e2' <- mapExpr f e2
    f (BinOp op t e1' e2')
mapExpr f (UnaryOp u t e) = do
    e' <- mapExpr f e
    f (UnaryOp u t e')
mapExpr f e@(Var _ _) = f e
mapExpr f (Assign t e1 e2) = do
    e1' <- mapExpr f e1
    e2' <- mapExpr f e2
    f (Assign t e1' e2')

mapStmt :: Monad m => (Expr_ a -> m (Expr_ a)) -> Stmt_ a-> m (Stmt_ a)
mapStmt f (ExprStmt e) = ExprStmt <$> mapExpr f e
mapStmt f (ReturnStmt e) = ReturnStmt <$> mapExpr f e
mapStmt f (CompoundStmt c) = do
    c' <- mapM (mapStmt f) c
    return $ CompoundStmt c'
mapStmt f (IfStmt e s ms) = do
    e' <- mapExpr f e
    s' <- mapStmt f s
    ms' <- traverse (mapStmt f) ms
    return $ IfStmt e' s' ms'
mapStmt f (ForStmt ms1 me1 me2 s) = do
    ms1' <- traverse (mapStmt f) ms1
    me1' <- traverse (mapExpr f) me1
    me2' <- traverse (mapExpr f) me2
    s' <- mapStmt f s
    return $ ForStmt ms1' me1' me2' s'

annotateExpr :: Expr -> Either CompilerError Expr
annotateExpr (IntLit i _) = return $ IntLit i CInt
annotateExpr (BinOp op _ e1 e2) = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    return $ BinOp op (cType e1') e1' e2'
annotateExpr (UnaryOp Addr _ e) = do
    e' <- annotateExpr e
    return $ UnaryOp Addr (CPtr $ cType e') e'
annotateExpr (UnaryOp DeRef _ e) = do
    e' <- annotateExpr e
    case cType e' of
        CPtr k -> return $ UnaryOp DeRef k e'
        _      -> return $ UnaryOp DeRef CInt e' -- will be caught in semantic check
annotateExpr (UnaryOp op _ e) = do
    e' <- annotateExpr e
    return $ UnaryOp op CInt e'
annotateExpr (Var v _) = return $ Var v CInt
annotateExpr (Assign _ e1 e2) = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    return $ Assign (cType e1') e1' e2'

pointerArithmetic :: Expr -> Either CompilerError Expr
pointerArithmetic (BinOp Add t a b) = do
    let ta = cType a
    let tb = cType b
    case (ta, tb) of
        (CInt, CInt) -> return $ BinOp Add t a b
        (CPtr _, CInt) -> return $
            BinOp Add t a (BinOp Mul CInt b (IntLit 8 CInt))
        (CInt, CPtr _) -> return $ BinOp Add t b a
        _ -> Left $ SemanticError "ptr cannot add ptr"
pointerArithmetic (BinOp Sub t a b) = do
    let ta = cType a
    let tb = cType b
    case (ta, tb) of
        (CInt, CInt) -> return $ BinOp Sub t a b
        (CPtr _, CInt) -> return $
            BinOp Sub t a (BinOp Mul CInt b (IntLit 8 CInt))
        (CPtr _, CPtr _) -> return $
            BinOp Div CInt (BinOp Sub CInt a b) (IntLit 8 CInt)
        _ -> Left $ SemanticError "ptr cannot sub int"
pointerArithmetic (BinOp op t a b) = return $ BinOp op t a b
pointerArithmetic (UnaryOp op t e) = return $ UnaryOp op t e
pointerArithmetic e = return e

semantic :: ([Stmt], [LocalVal]) -> Either CompilerError ([Stmt], [LocalVal])
semantic (stmts, l) = do
    stmts <- mapM (mapStmt annotateExpr) stmts
    stmts <- mapM (mapStmt pointerArithmetic) stmts
    return (stmts, l)

showSemantic :: ([Stmt], [LocalVal]) -> Either CompilerError String
showSemantic (stmts, _) = return $ unlines $ map show stmts