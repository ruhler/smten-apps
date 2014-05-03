
-- | Abstract syntax tree for a Sketch program.
module Syntax (
    Type(..), Name,
    LVal(..), Stmt(..),
    Pointer(..), Value(..), BinOp(..), Expr(..), Arg(..), Function(..),
    Member(..),
    functionT,
    blockS, typeofV, arrayV, nullV, pad, binaryChoiceE, fieldChoiceE,
    asLVal, asType,
    ) where

import Smten.Prelude
import Smten.Control.Monad

import Bits

type Name = String

-- Type
--
-- Note: The type (StructT "") is used to represent a type
-- which is known to be a struct type, but which specific struct type
-- is unknown.
--
-- Note: Equality is structural equality.
-- This means, for example, bit[3] is not (==) to bit[2+1]
data Type = 
    VoidT             -- void
  | BitT              -- bit
  | ArrT Type Expr    -- T[n]
  | IntT              -- int
  | FunT Type [Type]  -- function type: output and argument types
  | StructT Name         -- Foo
  | UnknownT          -- type is not known
    deriving (Eq, Show)


data Pointer = Null | Pointer Int
    deriving (Eq, Show)

data Value = 
    ArrayV [Value]
  | BitV Bit
  | BitsV Bits
  | IntV Int
  | FunV Function
  | VoidV
  | PointerV Pointer
    deriving (Eq, Show)

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

nullV :: Value
nullV = PointerV Null

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
typeofV (PointerV {}) = StructT ""

-- | Binary operators
data BinOp = 
   AndOp        -- ^ a & b
 | LAndOp       -- ^ a && b
 | AddOp        -- ^ a + b
 | SubOp        -- ^ a - b
 | LtOp         -- ^ a < b
 | GtOp         -- ^ a > b
 | LeOp         -- ^ a <= b
 | GeOp         -- ^ a >= b
 | EqOp         -- ^ a == b
 | NeqOp        -- ^ a != b
 | OrOp         -- ^ a | b
 | LOrOp        -- ^ a || b
 | XorOp        -- ^ a ^ b
 | MulOp        -- ^ a * b
 | ModOp        -- ^ a % b
 | DivOp        -- ^ a / b
 | ShrOp        -- ^ a >> b
 | ShlOp        -- ^ a << b
 | ChoiceOp [BinOp] -- ^ (op | op | ... | op)
    deriving (Show)

instance Eq BinOp where
 (==) AndOp              AndOp  = True
 (==) LAndOp             LAndOp = True
 (==) AddOp              AddOp  = True
 (==) SubOp              SubOp  = True
 (==) LtOp               LtOp   = True
 (==) GtOp               GtOp   = True
 (==) LeOp               LeOp   = True
 (==) GeOp               GeOp   = True
 (==) EqOp               EqOp   = True
 (==) NeqOp              NeqOp  = True
 (==) OrOp               OrOp   = True
 (==) LOrOp              LOrOp  = True
 (==) XorOp              XorOp  = True
 (==) MulOp              MulOp  = True
 (==) ModOp              ModOp  = True
 (==) DivOp              DivOp  = True
 (==) ShrOp              ShrOp  = True
 (==) ShlOp              ShlOp  = True
 (==) (ChoiceOp xs)      (ChoiceOp ys) = xs == ys
 (==) _ _ = False


data Expr = 
   ValE Value
 | BinaryE BinOp Expr Expr  -- ^ a <op> b
 | CondE Expr Expr Expr  -- ^ p ? a : b
 | NotE Expr             -- ^ ! a
 | PostIncrE LVal        -- ^ x++
 | PostDecrE LVal        -- ^ x--
 | PreIncrE LVal         -- ^ ++x
 | PreDecrE LVal         -- ^ --x
 | PlusEqE LVal Expr     -- ^ x += e
 | ArrayE [Expr]         -- ^ {a, b, ... }
 | HoleE Type (Maybe Int)       -- ^ ??(n)      n is the number of bits to use
 | ChoiceE Expr Expr     -- ^ {| a | b |}
 | BitChooseE Type Expr Expr    -- ^ a {|} b
 | VarE Name             -- ^ foo
 | AccessE Expr Expr     -- ^ foo[i]    Note: i has type Int
 | BulkAccessE Expr Expr Expr   -- ^ foo[lo::N]
 | FieldE Expr Name      -- ^ foo.bar
 | NewE Name [(Name, Expr)]     -- ^ new Foo(x=..., y=..., ...)
 | CastE Type Expr       -- ^ (T) e
 | ICastE Type Expr      -- ^ implicit cast of an expr to a given type
 | AppE Name [Expr]      -- ^ f(x, y, ...)
    deriving (Eq, Show)

data LVal = VarLV Name                  -- foo
          | ArrLV LVal Expr             -- foo[i]
          | BulkLV LVal Expr Expr       -- foo[lo::N]
          | FieldLV LVal Name           -- foo.bar
          | ChoiceLV LVal LVal          -- {| foo | bar |}
    deriving (Eq, Show)

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
asLVal (ChoiceE a b) = liftM2 ChoiceLV (asLVal a) (asLVal b)
asLVal _ = Nothing

-- Types are parsed as expressions to avoid conflicts. This
-- converts a type represented as an Expr to a Type
asType :: Expr -> Maybe Type
asType (VarE "bit") = Just BitT
asType (VarE "void") = Just VoidT
asType (VarE "int") = Just IntT
asType (VarE nm) = Just (StructT nm)
asType (AccessE a m) = do
  ta <- asType a
  return (ArrT ta m)
asType _ = Nothing

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data Function = Function {
    f_outtype :: Type,
    f_args :: [Arg],
    f_body :: Stmt
} deriving (Eq, Show)

-- | Return the type of a function.
functionT :: Function -> Type
functionT f = FunT (f_outtype f) [t | Arg _ t _ <- f_args f]

pad :: Type -> Value
pad BitT = BitV False
pad IntT = IntV 0
pad (ArrT t (ValE (IntV w))) = arrayV (replicate w (pad t))
pad (StructT _) = PointerV Null

binaryChoiceE :: [BinOp] -> Expr -> Expr -> Expr
binaryChoiceE [] a b = error "binaryChoiceE: no operators given"
binaryChoiceE [x] a b = BinaryE x a b
binaryChoiceE xs a b = BinaryE (ChoiceOp xs) a b

data Member =
   FieldM Name 
 | SubM Member Name

fieldE :: Expr -> Member -> Expr
fieldE x (FieldM nm) = FieldE x nm
fieldE x (SubM m nm) = FieldE (fieldE x m) nm

fieldChoiceE :: Expr -> [Member] -> Expr
fieldChoiceE e [] = error "fieldChoiceE: no fields given"
fieldChoiceE e [m] = fieldE e m
fieldChoiceE e (m:ms) = ChoiceE (fieldE e m) (fieldChoiceE e ms)

