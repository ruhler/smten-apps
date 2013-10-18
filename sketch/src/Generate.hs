
module Generate (generate) where

import Smten.Prelude
import Smten.Data.Functor
import Smten.Symbolic

import Input
import Sketch

-- Given a program with explicit holes, generate a corrisponding symbolic
-- candidate program without holes.
generate :: Prog -> Symbolic Prog
generate = mapM generateD

generateD :: Decl -> Symbolic Decl
generateD d@(VarD {}) = return d
generateD d@(FunD {}) = do
    stmts' <- deHoleStmts (fd_stmts d)
    return $ d { fd_stmts = stmts' }

deHoleStmts :: [Stmt] -> Symbolic [Stmt]
deHoleStmts = mapM deHoleStmt

deHoleStmt :: Stmt -> Symbolic Stmt
deHoleStmt (ReturnS x) = ReturnS <$> deHoleExpr x
deHoleStmt s@(DeclS ty nm) = return s
deHoleStmt (UpdateS nm e) = UpdateS nm <$> deHoleExpr e
deHoleStmt (ArrUpdateS nm idx e) = do
    idx' <- deHoleExpr idx
    e' <- deHoleExpr e
    return (ArrUpdateS nm idx' e')
deHoleStmt (IfS p a b) = do
    p' <- deHoleExpr p
    a' <- deHoleStmt a
    b' <- deHoleStmt b
    return (IfS p' a' b')
deHoleStmt (BlockS xs) = BlockS <$> mapM deHoleStmt xs

deHoleExpr :: Expr -> Symbolic Expr
deHoleExpr (AndE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (AndE a' b')
deHoleExpr (AddE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (AddE a' b')
deHoleExpr (ArrayE a) = do
    a' <- mapM deHoleExpr a
    return (ArrayE a')
deHoleExpr (XorE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (XorE a' b')
deHoleExpr (MulE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (MulE a' b')
deHoleExpr (OrE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (OrE a' b')
deHoleExpr (ShlE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (ShlE a' b')
deHoleExpr (ShrE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (ShrE a' b')
deHoleExpr (NotE a) = NotE <$> deHoleExpr a
deHoleExpr (HoleE ty) = mkFreeArg ty
deHoleExpr x@(BitE {}) = return x
deHoleExpr x@(BitsE {}) = return x
deHoleExpr x@(IntE {}) = return x
deHoleExpr x@(VarE {}) = return x
deHoleExpr (AccessE a b) = do
    a' <- deHoleExpr a
    b' <- deHoleExpr b
    return (AccessE a' b')

