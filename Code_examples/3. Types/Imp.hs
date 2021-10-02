-- | A small imperative programming language.
module Imp where

import Data.Map (Map,fromList,lookup,insert)
import Prelude hiding (lookup)


--
-- * Syntax
--

-- ** Abstract syntax

--  var   ::=  (any variable name)
--
--  type  ::=  `int`  |  `bool`
--
--  decl  ::=  var : type
-- 
--  prog  ::=  decl* `begin` stmt
--
--  expr  ::=  int
--         |   expr + expr
--         |   expr * expr
--         |   expr ≤ expr
--         |   `not` expr
--         |   expr & expr
--         |   expr | expr
--         |   var
--
--  stmt  ::=  var := expr
--         |   `if` expr stmt `else` stmt
--         |   `while` expr stmt
--         |   { stmt* }

-- | Variables.
type Var = String

-- | Abstract syntax of types.
data Type = TInt | TBool
  deriving (Eq,Show)

-- | Abstract syntax of declarations.
type Decl = (Var,Type)

-- | Abstract syntax of programs.
data Prog = P [Decl] Stmt
  deriving (Eq,Show)

-- | Abstract syntax of expressions.
data Expr
   = Lit Int         -- literal integer
   | Add Expr Expr   -- integer addition
   | Mul Expr Expr   -- integer multiplication
   | LTE Expr Expr   -- less than or equal to
   | Not Expr        -- boolean negation
   | And Expr Expr   -- boolean conjunction
   | Or  Expr Expr   -- boolean disjunction
   | Ref Var         -- variable reference
  deriving (Eq,Show)

-- | Abstract syntax of statements.
data Stmt
   = Bind Var Expr       -- variable assignment
   | If Expr Stmt Stmt   -- conditional statement
   | While Expr Stmt     -- while loop
   | Block [Stmt]        -- statement block
  deriving (Eq,Show)


-- ** Syntactic sugar

-- | Literal boolean values.
true, false :: Expr
true  = LTE (Lit 0) (Lit 1)
false = LTE (Lit 1) (Lit 0)

-- | Integer negation.
neg :: Expr -> Expr
neg e = Mul (Lit (-1)) e

-- | Integer subtraction.
sub :: Expr -> Expr -> Expr
sub l r = Add l (neg r)

-- | Comparison operations.
eq, neq, lt, gt, gte :: Expr -> Expr -> Expr
eq  l r = And (LTE l r) (LTE r l)
neq l r = Not (eq l r)
lt  l r = And (LTE l r) (Not (eq l r))
gt  l r = Not (LTE l r)
gte l r = Or  (gt l r) (eq l r)


-- ** Example programs

-- | Generate a program that sums all of the numbers from n to m.
--   For example, genSum 1 100:
--
--     sum : int
--     n : int
--     m : int
--     begin {
--       sum := 0
--       n := 1
--       m := 100
--       while n <= m {
--         sum := sum + n
--         n := n + 1
--       }
--     }
--
genSum :: Int -> Int -> Prog
genSum n m =
    P [("sum",TInt),("n",TInt),("m",TInt)]
      (Block [
        Bind "sum" (Lit 0),
        Bind "n" (Lit n),
        Bind "m" (Lit m),
        While (LTE (Ref "n") (Ref "m"))
        (Block [
          Bind "sum" (Add (Ref "sum") (Ref "n")),
          Bind "n" (Add (Ref "n") (Lit 1))
        ])
      ])


-- | Generate a program that computes the greatest common divisor of a and b.
--   For example, genGCD 306 657
--
--     a : int
--     b : int
--     c : int
--     begin {
--       a := 306
--       b := 657
--       while a != b {
--         while a > b {
--           c = a - b
--           a = c
--          }
--          while b > a {
--            c = b - a
--            b = c
--          }
--       }
--     }
genGCD :: Int -> Int -> Prog
genGCD a b =
    P [("a",TInt),("b",TInt),("c",TInt)]
      (Block [
        Bind "a" (Lit a),
        Bind "b" (Lit b),
        While (neq (Ref "a") (Ref "b"))
        (Block [
          While (gt (Ref "a") (Ref "b"))
          (Block [
            Bind "c" (sub (Ref "a") (Ref "b")),
            Bind "a" (Ref "c")
          ]),
          While (gt (Ref "b") (Ref "a"))
          (Block [
            Bind "c" (sub (Ref "b") (Ref "a")),
            Bind "b" (Ref "c")
          ])
        ])
      ])


-- ** Examples of programs with type errors.

-- x : int
-- begin
--   x := 3 ≤ 4
bad1 :: Prog
bad1 = P [("x",TInt)] (Bind "x" (LTE (Lit 3) (Lit 4)))

-- b : bool
-- begin
--   b := 3 + 4
bad2 :: Prog
bad2 = P [("b",TBool)] (Bind "b" (Add (Lit 3) (Lit 4)))

-- x : int
-- begin
--   x := 3 + true
bad3 :: Prog
bad3 = P [("x",TInt)] (Bind "x" (Add (Lit 3) true))

-- begin
--   if 3 * 4 {} else {}
bad4 :: Prog
bad4 = P [] (If (Mul (Lit 3) (Lit 4)) (Block []) (Block []))

-- begin
--   while 3 * 4 {}
bad5 :: Prog
bad5 = P [] (While (Mul (Lit 3) (Lit 4)) (Block []))

bads :: [Prog]
bads = [bad1,bad2,bad3,bad4,bad5]


--
-- * Type system
--

-- | Variable environments. An environment maps variable names to the
--   things those variable names are bound to. During typing, each name
--   will be bound to a type, while during evaluation (semantics), each
--   name will be bound to a value.
--
--   Operations on maps:
--     * lookup :: Var -> Env a -> Maybe a
--     * insert :: Var -> a -> Env a -> Env a
type Env a = Map Var a

-- | Typing relation for expressions. We need an environment to lookup
--   the names of variable references (last case). We use a Maybe to
--   represent the fact that typing might fail, for example, if we get
--   a type error or if a variable is not in the environment.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)   _ = Just TInt
typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _ -> Nothing
typeExpr (Mul l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _ -> Nothing
typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TBool
                         _ -> Nothing
typeExpr (Not e)   m = case typeExpr e m of
                         Just TBool -> Just TBool
                         _ -> Nothing
typeExpr (And l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TBool, Just TBool) -> Just TBool
                         _ -> Nothing
typeExpr (Or  l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TBool, Just TBool) -> Just TBool
                         _ -> Nothing
typeExpr (Ref v)   m = lookup v m


-- | Type checking statements. Note that the return type here is just a
--   Boolean value since a statement doesn't have a type. The Boolean
--   value indicates whether or not the statement is type correct (i.e.
--   this function implements type checking of statements).
-- 
--   Also note that when we type check a while loop, we do not execute
--   the loop several times; we just check that the type of the condition
--   is a Bool, and check that the body type checks. If both of those are
--   true, then we know that the while loop cannot produce a type error
--   without having to consider each iteration. This is where the benefits
--   of static type checking start to become more clear. With dynamic
--   typing we would have to check the types in the loop body on each
--   iteration, whereas with static typing we just check the loop body
--   once.
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)  m = case (lookup v m, typeExpr e m) of
                           (Just tv, Just te) -> tv == te
                           _ -> False
typeStmt (If c t e)  m = typeExpr c m == Just TBool && typeStmt t m && typeStmt e m
typeStmt (While c b) m = typeExpr c m == Just TBool && typeStmt b m
typeStmt (Block ss)  m = all (\s -> typeStmt s m) ss


-- | Type checking programs. The 'fromList' function is from the
--   Data.Map module. It builds a map from a list of pairs, thus
--   initializing our typing environment.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)


--
-- * Semantics
--

-- | The basic values in our language.
type Val = Either Int Bool

-- | Semantics of type-correct expressions. Note that since we assume the
--   expression is statically type correct (otherwise it would have failed
--   type checking and we never try to evaluate it), we do not need to
--   explicitly represent the error case with a Maybe type. Also note that
--   our environment now contains the *value* that each name is bound to.
--   Since expressions can refer to variables but not change them, the
--   environment is read-only (i.e. it's an input but not an output of the
--   function).
evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (Mul l r) m = Left (evalInt l m * evalInt r m)
evalExpr (LTE l r) m = Right (evalInt l m <= evalInt r m)
evalExpr (Not e)   m = Right (not (evalBool e m))
evalExpr (And l r) m = Right (evalBool l m && evalBool r m)
evalExpr (Or  l r) m = Right (evalBool l m || evalBool r m)
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"


-- | Helper function to evaluate an expression to an integer. Note that
--   in all cases, we should only get an "internal error" if we try to
--   evaluate an expression that didn't pass the static type checker we
--   wrote above.
evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"


-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"


-- | Semantics of statements. Statements update the bindings in the
--   environment, so the semantic domain is 'Env Val -> Env Val'. The
--   bind case is the case that actually changes the environment. The
--   other cases should look similar to other examples you've seen.
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)  m = insert x (evalExpr e m) m
evalStmt (If c t e)  m = if evalBool c m
                         then evalStmt t m
                         else evalStmt e m
evalStmt (While c b) m = if evalBool c m
                         then evalStmt (While c b) (evalStmt b m)
                         else m
evalStmt (Block ss)  m = evalStmts ss m


-- | Helper function to evaluate a list of statements. We could also
--   have used 'foldl' here.
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)


-- | Semantics of programs. This runs a program with an initial
--   environment where all integer variables are initialized to 0, and
--   all Boolean variables are initialized to false.
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

-- | Type check and then run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing
