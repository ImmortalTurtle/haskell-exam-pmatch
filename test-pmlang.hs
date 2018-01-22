module Main where

import PMParser (parseScrutinee, parseCase)
import Test.HUnit
import Data.Either
import Data.Hashable (hash)
import System.Exit
import PMEval

wrap name what casesS expected =
  TestLabel name $ TestList $ [c1] ++ c2 ++ [c3]
  where
    w = PMParser.parseScrutinee optsE what
    c1 = TestCase (assertBool "parsable" (isRight w))
    cases = map (PMParser.parseCase optsP optsE) casesS
    c2 = map (\r -> TestCase (assertBool "case parsable" (isRight r)) ) cases
    (_,cases2) = partitionEithers cases
    c3 = TestCase (assertEqual "evaluatedRight"
                      (PMEval.eval w cases) expected)

-- mistakes in syntax can be here. Please report the ones.
tests = TestList
  [ wrap "test1" "C(1)"     ["_ -> 42", "x->x"] (OK 42)
  , wrap "test2" "0+14*3"   ["_ -> 42"] (OK 42)
  , wrap "test3" "A(1,2)"   ["A(x,y) -> 42"] (OK 42)
  , wrap "test4" "A(42,1)"  ["A(x,_) -> x", "A(_,x) -> x", "_->42" ] (OK 42)
  , wrap "test5" "A(1,43)"  ["x -> (field 1 x) - (field 0 x)"] (OK 42)
  , wrap "test6" "P(1,2,6)" ["x -> if (field 0 x) < (field 1 x) then 42 else 42"] (OK 42)
  , wrap "test7" "P(10,10)" ["P(x,y) -> if 6<x then (if 19<y then 42 else 19) else 34" ] (OK 42)
  -- next should fail with silly eval
  --, wrap "test3" "A(B,C)"   ["A(x,y) -> (tag x) + (tag y)"] (OK $ hash "B" + hash "C")
  ]

main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
   then
     exitWith ExitSuccess
   else
     exitWith (ExitFailure 1)