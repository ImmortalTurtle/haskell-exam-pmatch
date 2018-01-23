module PMEval where

import PMParser
import Data.Hashable (hash)
import Text.Parsec as Parsec

data Expr = Const Int | Tag Expr | Var String |
            Field Int Expr | BinOp BinOpSort Expr Expr | 
            Econstr String [Expr] | Ifthenelse Expr Expr Expr |
            BoolExpr Bool
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

--takes 2 constants and returns BoolExpr
-- bincalc :: BinOpSort -> Expr -> Expr -> Expr
-- bincalc (Const bool) = BoolExpr True

reduce1 :: Expr -> Maybe Expr
reduce1 (Const _) = Nothing 
-- reduce1 (Tag string) = Just (Const $ hash string)
-- reduce1 (Field index (Econstr string args)) = args !! index --not Econstr | len(args) < index -> BadProgram
reduce1 (Var _) = Nothing --substitute
-- reduce1 (BinOp oper e1 e2) = reduce em, if not const then error
-- reduce1 (Ifthenelse (BoolExpr cond)  e1 e2) = if cond then e1 else e2 -- we might
reduce1 _ = Nothing --const, var

simplify ::(Expr -> Maybe Expr) ->  Expr -> Expr
simplify reduce1 e = case reduce1 e of
                          Just e -> simplify reduce1 e
                          Nothing -> e 

-- ((Expr, Pattern) -> Bool) -> [(Expr, Pattern)] -> Bool
-- (a -> Bool) -> [a] -> Bool
-- curry match' 
--check if expr matches pattern, reduce if needed
match' :: Expr -> Pattern -> Bool
match' _ Wild = True
match' (Const e) (Pconst p) = e == p
match' (Econstr ename eargs) (Pconstr pname pargs) = ename == pname && length eargs == length pargs
                                                     && (all (uncurry match') (zip eargs pargs))
match' e p = False
--all match cases
--if not found, try to reduce Expr
--not reducable => return False

--this is correct pattern, just calculate the expression, which is already simplified
calculate :: (Pattern, Expr) -> EvalRez
calculate (Wild, _) = OK 43 --testWild
calculate (Pconstr _ _, e) = OK 41 --testConstr
calculate (p, e) = OK 42 


eval ::  Expr -> [(Pattern, Expr)] -> EvalRez
eval _ [] = PMatchFail
eval expr ((lhs,rhs) : ps) = 
    if (match' expr lhs) then (calculate (lhs,rhs))
    else eval expr ps
    -- simplify all expressions