module Main where

import PMParser
import PMEval (optsE, optsP)


rhs = [ "1+2*3", "x+y*3", "1+(field 1 x)+3", "(tag x) * (field 18 y)"
      , "if 0<x then (if 0<y then 42 else 19) else 34"
      ]
patts = [ "C _", "_", "C(x,y)", "C(D,E(x))", "U(_)" ]
cases = [ "_ -> 1"
        , "x -> x"
        , "_ -> 1-2"
        , "C(x,y)-> if ((tag x)=(tag y)) then 1 else 2"
        , "P(x,y) -> if 6<x then (if 19<y then 42 else 19) else 34"
        , "A(2,x) -> 42"
        , "P _ -> 42"
        ]
scrutinees = [ "A(1)", "1"]

main = do
  print "playground is here"
  mapM_ (\line -> print line >> print (PMParser.parseRhs  optsE    line))
    rhs
  mapM_ (\line -> print line >> print (PMParser.parsePatt optsP    line))
    patts
  -- mapM_ (\line -> print line >> print (PMParser.parseCase optsP optsE line))
  --   cases
  -- mapM_ (\line -> print line >> print (PMParser.parseScrutinee optsE line))
  --   scrutinees
