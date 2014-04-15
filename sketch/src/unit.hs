
import Smten.Prelude
import Smten.Tests.Test

import Syntax
import Ppr2

main :: IO ()
main = do
    -- Test pretty printing of types
    test "void" ("void" == pretty VoidT)
    test "bit" ("bit" == pretty BitT)
    test "int" ("int" == pretty IntT)
    test "struct" ("Foo" == pretty (StructT "Foo"))

    -- Test pretty printing of values
    test "bitv T" ("true" == pretty (BitV True))
    test "bitv F" ("false" == pretty (BitV False))
    test "intv" ("4" == pretty (IntV 4))
    test "null" ("null" == pretty (PointerV Null))
    test "arrayv" ("{1,4,2,6}" == pretty (ArrayV [IntV 1, IntV 4, IntV 2, IntV 6]))

    -- Test pretting printing of expressions
    test "1+2*3" ("1 + 2 * 3" == pretty (AddE (ValE (IntV 1)) (MulE (ValE (IntV 2)) (ValE (IntV 3)))))
    test "(1+2)*3" ("(1 + 2) * 3" == pretty (MulE (AddE (ValE (IntV 1)) (ValE (IntV 2))) (ValE (IntV 3))))
    test "1-2-3" ("1 - 2 - 3" == pretty (SubE (SubE (ValE (IntV 1)) (ValE (IntV 2))) (ValE (IntV 3))))
    test "1-(2-3)" ("1 - (2 - 3)" == pretty (SubE (ValE (IntV 1)) (SubE (ValE (IntV 2)) (ValE (IntV 3)))))
    test "1+(2+3)" ("1 + 2 + 3" == pretty (AddE (ValE (IntV 1)) (AddE (ValE (IntV 2)) (ValE (IntV 3)))))

    putStrLn "Sketch UNIT PASSED"

