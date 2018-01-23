module PMEval where

import PMParser

data Expr = Const Int | Tag String | Var String |
            Field Int String | BinOp BinOpSort Expr Expr | 
            Econstr String [Expr] | Ifthenelse Expr Expr Expr
            deriving (Show)

data Pattern = Wild | Pconstr String [Pattern] |
               Named String | Pconst Int 
               deriving (Show)


-- next two structures are used by parser
optsE :: Ops Expr
optsE = TermConstruction 
    { int = const Const
    , tag = const Tag
    , var = const Var
    , field = const Field
    , binop = BinOp
    , econstr = const Econstr
    , ifthenelse = const Ifthenelse
    }

optsP :: PattOps Pattern
optsP = PattConstruction
    { wild = const Wild
    , pconstr = const Pconstr
    , named = const Named
    , pconst = const Pconst
    }

data EvalRez =
    OK Int           -- success
  | BadProgram       -- adding integers to functions, getting tag of integer,
                     -- unbound variables, etc.
  | PMatchFail       -- patterns are not exhaustive.
  deriving (Show,Eq)
      
-- implement this function :: expr -> [(pattern,expr)] -> EvalRez
-- which tries to match `expr` using specified patterns and right-hand-sides
-- and returns appropritate answer. For, example
--    eval `parseScrutinee optsE "A"` [`parseCase optsp optsE "A -> 42"`]
--      should return (OK 42)
-- and
--    eval `parseScrutinee optsE "A"` []
--      should fail with PMatchFail because pattern matching is not exhaustive.

-- For examples about which expression and patterns can be written see tests file.
eval what cases = OK 42



