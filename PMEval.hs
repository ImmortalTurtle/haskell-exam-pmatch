module PMEval where

import PMParser
import Data.Hashable (hash)
import Text.Parsec as Parsec

data Expr = Const Int | Tag Expr | Var String |
            Field Int Expr | BinOp BinOpSort Expr Expr | 
            Econstr String [Expr] | Ifthenelse Expr Expr Expr |
            BoolExpr Bool | InvalidExpr
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

-- takes 2 constants and returns Const Int, BoolExpr or InvalidExpr in case of error
bincalc :: BinOpSort -> Expr -> Expr -> Expr
bincalc oper (Const x) (Const y) = case oper of
    Mul -> Const $ x * y
    Add -> Const $ x + y
    Sub -> Const $ x - y
    Eq -> BoolExpr $ x == y
    LessThen -> BoolExpr $ x <= y
    LessEq -> BoolExpr $ x < y
bincalc _ _ _ = InvalidExpr

--
subst :: Expr -> (Pattern, Expr) -> Expr
subst what (Named var, Const num) = Const num
subst what (Named pvar, Var evar) = if pvar == evar then what else (Var evar)
subst what (Named var, Tag tag) = Tag $ subst what (Named var, tag)
subst what (Named var, BinOp oper e1 e2) = BinOp oper (subst what (Named var, e1)) (subst what (Named var, e2))
subst what (Named var, Field index expr) = Field index (subst what (Named var, expr))
subst what (Named var, Econstr name args) = Econstr name $ map (curry (subst what) (Named var)) args
subst what (Named var, Ifthenelse cond e1 e2) = Ifthenelse (subst what (Named var, cond))
                                                           (subst what (Named var, e1))
                                                           (subst what (Named var, e2))
-- subst (Econstr name eargs) ((Pconstr name pargs), e) = Econstr 

subst what (p, e) = e

reduce1 :: Expr -> Maybe Expr
reduce1 (Const _) = Nothing 
reduce1 (Var _) = Nothing --substitute
reduce1 (Tag (Econstr name _)) = Just (Const $ hash name)
reduce1 (Field index cons@(Econstr string args)) = case lookup' args index of  
    Just e -> Just e
    Nothing -> Just InvalidExpr
reduce1 (Field index _) = Just InvalidExpr
reduce1 (BinOp oper e1 e2) = Just (bincalc oper (simplify e1) (simplify e2))
reduce1 (Ifthenelse (BoolExpr condition) e1 e2) = if condition then Just e1 else Just e2 
reduce1 (Ifthenelse condexpr e1 e2) = case (reduce1 condexpr) of
    Just (BoolExpr bool)-> if bool then Just e1 else Just e2
    -- InvalidExpr -> Just InvalidExpr
    _ -> Nothing
reduce1 _ = Just InvalidExpr


--copypaste
lookup' :: [a] -> Int -> Maybe a
lookup' [] _ = Nothing
lookup' (a:as) 0 = Just a
lookup' (a:as) n = lookup' as (n - 1)


simplify :: Expr -> Expr
simplify InvalidExpr = InvalidExpr
simplify e = case reduce1 e of
                 Just e -> simplify e
                 Nothing -> e 

--check for Expr-Pattern match
match' :: Expr -> Pattern -> Bool
match' _ Wild = True
match' (Const e) (Pconst p) = e == p
match' (Const e) (Named var) = True
match' (Econstr ename eargs) (Pconstr pname pargs) = ename == pname && length eargs == length pargs
                                                     && (all (uncurry match') (zip eargs pargs))
match' x (Named var) = True
match' InvalidExpr _ = False --or True and then able to throw BadProgram
match' e p = case reduce1 e of
    Nothing -> False
    Just e -> match' e p
match' _ _ = False

--called if match succeeded
calculate :: Expr -> (Pattern, Expr) -> EvalRez
calculate what (p, e) = case simplify (subst what (p, e)) of
    Const a -> OK a
    InvalidExpr -> BadProgram
    _ -> PMatchFail 

--main
eval ::  Expr -> [(Pattern, Expr)] -> EvalRez
eval _ [] = PMatchFail
eval expr ((lhs,rhs) : ps) = 
    if (match' expr lhs) then (calculate expr (lhs,rhs))
    else eval expr ps