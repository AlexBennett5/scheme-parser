import Parser
import Syntax
import Test.HUnit

parseTest :: String -> String -> [Expr] -> Test
parseTest name code expr = TestCase $ assertEqual name expr (parseProgram code)

tests :: Test
tests = TestList [ TestCase $ assertEqual "canary" 1 1
                 , parseTest "(3)" "(3)" [Num 3]
                 , parseTest "(examplevar)" "(examplevar)" [Call "examplevar"]
                 , parseTest "(+ 1 2)" "(+ 1 2)" [Application (Call "+") [Num 1, Num 2]]
                 , parseTest "(define variable 34)" "(define variable 34)" [Var "variable" (Num 34)]
                 , parseTest "(define variable '(* 1 2))" "(define variable '(* 1 2))" [Var "variable" (Quote (Application (Call "*") [Num 1, Num 2]))]
                 , parseTest "(define (function x y) (- x y))" "(define (function x y) (- x y))" [Func "function" ["x", "y"] (Application (Call "-") [Call "x", Call "y"])]
                 , parseTest "'(function 3 56)" "'(function 3 56)" [Quote (Application (Call "function") [Num 3, Num 56])]
                 , parseTest "(lambda (x) (+ x 2))" "(lambda (x) (+ x 2))" [Lambda ["x"] (Application (Call "+") [Call "x", Num 2])]
                 , parseTest "(lambda (x y) (+ x y))" "(lambda (x y) (+ x y))" [Lambda ["x", "y"] (Application (Call "+") [Call "x", Call "y"])]
                 , parseTest "(if (func1 2) (func2 10) (- 10 5))" "(if (func1 2) (func2 10) (- 10 5))" [Cond (Application (Call "func1") [Num 2]) (Application (Call "func2") [Num 10]) (Application (Call "-") [Num 10, Num 5])]
                 , parseTest "(3)(4)" "(3)(4)" [Num 3, Num 4]
                 , parseTest "(< 2 3)(+ 1 2)" "(< 2 3)(+ 1 2)" [Application (Call "<") [Num 2, Num 3], Application (Call "+") [Num 1, Num 2]]]

main :: IO Counts
main = do runTestTT tests
