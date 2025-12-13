module Semantic where

import CompilerData
import Data.Generics.Uniplate.Data ( transformBiM )

cType :: Expr -> CType
cType (IntLit _ t)    = t
cType (BinOp _ t _ _) = t
cType (UnaryOp _ t _) = t
cType (Var _ t)       = t
cType (Assign t _ _)  = t

-- Traverse all Expr within Stmt
mapStmtExpr :: Monad m => (Expr -> m Expr) -> Stmt -> m Stmt
mapStmtExpr = transformBiM

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
annotateExpr f@(FunCall {}) = return f

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

semantic :: CFile -> Either CompilerError CFile
semantic (CFile funcs) = do
    funcs' <- mapM processFunction funcs
    return $ CFile funcs'
  where
    processFunction :: Function -> Either CompilerError Function
    processFunction f = do
        stmts1 <- mapM (mapStmtExpr annotateExpr) (body f)
        stmts2 <- mapM (mapStmtExpr pointerArithmetic) stmts1
        return f { body = stmts2 }

showSemantic :: CFile -> Either CompilerError String
showSemantic (CFile stmts) = return $ unlines $ map show stmts