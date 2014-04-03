
-- | Abstract syntax tree for a Sketch program.
module Syntax (
    Type(..), Name,
    LVal(..), Stmt(..),
    Expr(..), Value(..), Arg(..), Function(..),
    functionT,
    blockS, typeofV, dimension, arrayV, pad,
    asLVal,
    ) where

import Smten.Prelude

import Bits

type Name = String

data Type = 
    VoidT             -- void
  | BitT              -- bit
  | ArrT Type Expr    -- T[n]
  | IntT              -- int
  | FunT Type [Type]  -- function type: output and argument types
  | StructT Name         -- Foo
  | UnknownT          -- type is not known
    deriving (Show)

instance Eq Type where
    (==) VoidT VoidT = True
    (==) BitT BitT = True
    (==) (ArrT a (ValE (IntV aw))) (ArrT b (ValE (IntV bw))) =
        a == b && aw == bw
    (==) (ArrT a wa) (ArrT b wb)
       | a == b = error $ "type equality on arrays with unknown width: " ++ show (wa, wb)
    (==) IntT IntT = True
    (==) (FunT a as) (FunT b bs) = a == b && as == bs
    (==) (StructT a) (StructT b) = a == b
    (==) _ _ = False

data Value = 
    ArrayV [Value]
  | BitV Bit
  | BitsV Bits
  | IntV Int
  | FunV Function
  | VoidV
    deriving (Show)

instance Eq Value where
    (==) (ArrayV as) (ArrayV bs) = as == bs
    (==) (BitV a) (BitV b) = a == b
    (==) (BitsV a) (BitsV b) = a == b
    (==) (IntV a) (IntV b) = a == b
    (==) VoidV VoidV = True
    (==) a b = error $ "Value.==: bad args: " ++ show (a, b)

instance Ord Value where
    (<=) (IntV av) (IntV bv) = av <= bv
    (<=) a b = error $ "Value.<=: bad args: " ++ show (a, b)

    (<) (IntV av) (IntV bv) = av < bv
    (<) a b = error $ "Value.<: bad args: " ++ show (a, b)

    (>=) (IntV av) (IntV bv) = av >= bv
    (>=) a b = error $ "Value.>=: bad args: " ++ show (a, b)

    (>) (IntV av) (IntV bv) = av > bv
    (>) a b = error $ "Value.>: bad args: " ++ show (a, b)

instance Num Value where
    (+) (BitsV a) (BitsV b) = BitsV (a `addB` b)
    (+) (IntV a) (IntV b) = IntV (a + b)
    (+) a b = error $ "Value.+: bad args: " ++ show (a, b)

    (-) (BitsV a) (BitsV b) = BitsV (a `subB` b)
    (-) (IntV a) (IntV b) = IntV (a - b)
    (-) a b = error $ "Value.-: bad args: " ++ show (a, b)

    (*) (IntV a) (IntV b) = IntV (a * b)
    (*) a b = error $ "Value.+: bad args: " ++ show (a, b)

    abs = error $ "Value.abs: not supported"
    signum = error $ "Value.signum: not supported"
    fromInteger = error $ "Value.fromInteger: not supported"


-- Make an array value.
-- Automatically constructs a BitsV if the argument type is Bit.
arrayV :: [Value] -> Value
arrayV xs =
  case xs of
    (BitV {} : _) -> BitsV [v | BitV v <- xs]
    _ -> ArrayV xs

typeofV :: Value -> Type
typeofV (ArrayV []) = ArrT UnknownT (ValE (IntV 0))
typeofV (ArrayV xs) = ArrT (typeofV (head xs)) (ValE (IntV (length xs)))
typeofV (BitV {}) = BitT
typeofV (BitsV b) = ArrT BitT (ValE (IntV $ length b))
typeofV (IntV 0) = BitT     -- it may be a bit literal, so indicate that:
typeofV (IntV 1) = BitT     --  it will promote to int if needed
typeofV (IntV w) = IntT
typeofV (FunV f) = functionT f
typeofV VoidV = VoidT

-- Return the dimension of a type.
-- - void, bit, int, and function types have dimension 1.
-- - array types have dimension greater than 1
-- - The user should ensure the type passed to dimension is fully known. 
--   That is, it should contain no UnknownT
dimension :: Type -> Int
dimension VoidT = 1
dimension BitT = 1
dimension (ArrT t _) = 1 + dimension t
dimension IntT = 1
dimension (FunT {}) = 1
dimension (StructT {}) = 1
dimension UnknownT = error "dimension: UnknownT"


data Expr = 
   ValE Value
 | AndE Expr Expr        -- ^ a & b
 | LAndE Expr Expr       -- ^ a && b
 | AddE Expr Expr        -- ^ a + b
 | SubE Expr Expr        -- ^ a - b
 | LtE Expr Expr         -- ^ a < b
 | GtE Expr Expr         -- ^ a > b
 | LeE Expr Expr         -- ^ a <= b
 | GeE Expr Expr         -- ^ a >= b
 | EqE Expr Expr         -- ^ a == b
 | NeqE Expr Expr        -- ^ a != b
 | CondE Expr Expr Expr  -- ^ p ? a : b
 | OrE Expr Expr         -- ^ a | b
 | LOrE Expr Expr        -- ^ a || b
 | XorE Expr Expr        -- ^ a ^ b
 | MulE Expr Expr        -- ^ a * b
 | ModE Expr Expr        -- ^ a % b
 | DivE Expr Expr        -- ^ a / b
 | NotE Expr             -- ^ ! a
 | ShrE Expr Expr        -- ^ a >> b
 | ShlE Expr Expr        -- ^ a << b
 | PostIncrE LVal        -- ^ x++
 | PostDecrE LVal        -- ^ x--
 | PreIncrE LVal         -- ^ ++x
 | PreDecrE LVal         -- ^ --x
 | PlusEqE LVal Expr     -- ^ x += e
 | ArrayE [Expr]         -- ^ {a, b, ... }
 | HoleE Type (Maybe Int)   -- ^ ??(n)      n is the number of bits to use
 | BitChooseE Type Expr Expr  -- ^ a {|} b
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | BulkAccessE Expr Expr Expr -- ^ foo[lo::N]
 | FieldE Expr Name      -- ^ foo.bar
 | NewE Name             -- ^ new Foo()
 | CastE Type Expr       -- ^ (T) e
 | ICastE Type Expr      -- ^ implicit cast of an expr to a given type
 | AppE Name [Expr]      -- ^ f(x, y, ...)
    deriving (Show)

data LVal = VarLV Name                  -- foo
          | ArrLV LVal Expr             -- foo[i]
          | BulkLV LVal Expr Expr       -- foo[lo::N]
          | FieldLV LVal Name           -- foo.bar
    deriving (Show)

-- Convert an expression to its corresponding LVal.
asLVal :: Expr -> Maybe LVal
asLVal (VarE nm) = return $ VarLV nm
asLVal (AccessE arr idx) = do
    lv <- asLVal arr
    return (ArrLV lv idx)
asLVal (BulkAccessE arr lo w) = do
    lv <- asLVal arr 
    return (BulkLV lv lo w)
asLVal (FieldE x nm) = do
    lv <- asLVal x
    return (FieldLV lv nm)
asLVal _ = Nothing

data Stmt =
     ReturnS Expr                    -- ^ return e;
   | ExprS Expr                      -- ^ foo
   | AssertS Expr                    -- ^ assert e;
   | RepeatS Expr Stmt               -- ^ repeat (n) s
   | ReorderS [Stmt]                 -- ^ reorder { stmts }
   | WhileS Expr Stmt                -- ^ while (c) s
   | ForS Stmt Expr Stmt Stmt        -- ^ for (init ; cond ; incr ) body
   | DeclS Type [(Name, Maybe Expr)] -- ^ ty foo = x, bar = y, ...;
   | UpdateS LVal Expr               -- ^ foo = e;
   | IfS Expr Stmt Stmt              -- ^ if (e) s1 else s2
   | BlockS [Stmt]                   -- ^ { stmts }
    deriving (Show)

-- Construct a block of statements.
-- This flattens any blocks into the higher level.
blockS :: [Stmt] -> Stmt
blockS xs =
  let f :: Stmt -> [Stmt]
      f (BlockS xs) = concatMap f xs
      f s = [s]
  in BlockS (concatMap f xs)

-- Arg name type isref
--   isref - True if the argument is pass by reference.
data Arg = Arg Name Type Bool
    deriving (Show)

data Function = Function {
    f_outtype :: Type,
    f_args :: [Arg],
    f_body :: Stmt
} deriving (Show)

-- | Return the type of a function.
functionT :: Function -> Type
functionT f = FunT (f_outtype f) [t | Arg _ t _ <- f_args f]

pad :: Type -> Value
pad BitT = BitV False
pad IntT = IntV 0
pad (ArrT t (ValE (IntV w))) = arrayV (replicate w (pad t))

