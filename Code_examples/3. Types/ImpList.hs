-- | Adding lists to our small imperative programming language.
module Imp where

import Data.Map (Map,fromList,lookup,insert)
import Prelude hiding (lookup)


--
-- * Syntax
--

-- ** Abstract syntax

--  var   ::=  (any variable name)
--
--  type  ::=  `int`
--         |   `bool`  
--         |   [ type ]
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
--         |   [] : type       empty list of the indicated type
--         |   expr :: expr    cons
--         |   `head` expr     get the first element of a list
--         |   `tail` expr     get everything but the first of the list
--         |   `isNil` expr    is the list empty? 
--
--  stmt  ::=  var := expr
--         |   `if` expr stmt `else` stmt
--         |   `while` expr stmt
--         |   { stmt* }

-- Algebraic properties relating the constructors to the destructors,
-- showing that information is preserved.
--
--   l == head l :: tail l   (assuming l is not nil)
--   x == head (x :: l)
--   l == tail (x :: l)


-- | Variables.
type Var = String

-- | Abstract syntax of types.
data Type
   = TInt
   | TBool
   | TList Type
  deriving (Eq,Show)

-- | Abstract syntax of declarations.
type Decl = (Var,Type)

-- | Abstract syntax of programs.
data Prog = P [Decl] Stmt
  deriving (Eq,Show)

-- | Abstract syntax of expressions.
data Expr
   = Lit Int             -- literal integer
   | Add Expr Expr       -- integer addition
   | Mul Expr Expr       -- integer multiplication
   | LTE Expr Expr       -- less than or equal to
   | Not Expr            -- boolean negation
   | And Expr Expr       -- boolean conjunction
   | Or  Expr Expr       -- boolean disjunction
   | Ref Var             -- variable reference
   | Nil Type            -- empty list
   | Cons Expr Expr      -- prepend an element to a list
   | Head Expr           -- get the first element from a list
   | Tail Expr           -- get everything but the first element from a list
   | IsNil Expr          -- check whether a list is empty
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

-- | Literal integer lists.
intList :: [Int] -> Expr
intList []     = Nil (TList TInt)
intList (i:is) = Cons (Lit i) (intList is)

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


-- | Generate a program that sums all of the elements in a list.
--   For example, genSumList [1,2,3,4,5]
--
--     sum : int
--     list : [int]
--     begin {
--       sum := 0
--       list := 1 :: 2 :: 3 :: 4 :: 5 :: []
--       while not (isNil list) {
--         sum := sum + head list
--         list := tail list
--       }
--     }
--
genSumList :: [Int] -> Prog
genSumList is =
    P [("sum", TInt), ("list", TList TInt)]
      (Block [
        Bind "sum" (Lit 0),
        Bind "list" (intList is),
        While (Not (IsNil (Ref "list")))
        (Block [
          Bind "sum" (Add (Ref "sum") (Head (Ref "list"))),
          Bind "list" (Tail (Ref "list"))
        ])
      ])


-- | Generate a program that computes the greatest common divisor of a and b.
--   For example, genGCD 306 657:
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
--
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

-- x : int
-- begin
--   x := head x
bad6 :: Prog
bad6 = P [("x",TInt)] (Bind "x" (Head (Ref "x")))

-- x : int
-- begin
--   x := tail x
bad7 :: Prog
bad7 = P [("x",TInt)] (Bind "x" (Tail (Ref "x")))

-- x : int
-- begin
--   x := isNil x
bad8 :: Prog
bad8 = P [("x",TInt)] (Bind "x" (IsNil (Ref "x")))

-- l : [int]
-- begin
--   l := 0 :: 3
bad9 :: Prog
bad9 = P [("l",TList TInt)] (Bind "l" (Cons (Lit 0) (Lit 3)))

-- | All our bad programs.
bads :: [Prog]
bads = [bad1,bad2,bad3,bad4,bad5,bad6,bad7,bad8,bad9]


--
-- * Type system
--

-- | Variable environments.
type Env a = Map Var a

--   Operations on maps:
--     * lookup :: Var -> Env a -> Maybe a
--     * insert :: Var -> a -> Env a -> Env a

-- | Typing relation for expressions.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)         _ = Just TInt
typeExpr (Ref x)         m = lookup x m
typeExpr (Add l r)       m | isInt l m  && isInt r m  = Just TInt
typeExpr (Mul l r)       m | isInt l m  && isInt r m  = Just TInt
typeExpr (LTE l r)       m | isInt l m  && isInt r m  = Just TBool
typeExpr (Not e)         m | isBool e m               = Just TBool
typeExpr (And l r)       m | isBool l m && isBool r m = Just TBool
typeExpr (Or  l r)       m | isBool l m && isBool r m = Just TBool
typeExpr (Nil (TList t)) _ = Just (TList t)
typeExpr (Cons h t)      m = case (typeExpr h m, typeExpr t m) of
                               (Just th, Just (TList tt)) -> if th == tt
                                                             then Just (TList th)
                                                             else Nothing
                               _ -> Nothing
typeExpr (Head e)        m = case typeExpr e m of
                               Just (TList t) -> Just t
                               _ -> Nothing
typeExpr (Tail e)        m = case typeExpr e m of
                               Just (TList t) -> Just (TList t)
                               _ -> Nothing
typeExpr (IsNil e)       m = case typeExpr e m of
                               Just (TList _) -> Just TBool
                               _ -> Nothing
typeExpr _ _ = Nothing

-- | Check whether an expression is of type `TInt`.
isInt :: Expr -> Env Type -> Bool
isInt e m = typeExpr e m == Just TInt

-- | Check whether an expression is of type `TBool`.
isBool :: Expr -> Env Type -> Bool
isBool e m = typeExpr e m == Just TBool


-- | Type checking statements.
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind x e)  m = case (lookup x m, typeExpr e m) of
                           (Just tx, Just te) -> tx == te
                           _ -> False
typeStmt (If c t e)  m = isBool c m && typeStmt t m && typeStmt e m
typeStmt (While c b) m = isBool c m && typeStmt b m
typeStmt (Block ss)  m = all (\s -> typeStmt s m) ss


-- | Type checking programs.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)


--
-- * Semantics
--

-- | The basic values in our language.
data Value
   = I Int
   | B Bool
   | L [Value]
  deriving (Eq,Show)

-- | A store is an updatable environment of values.
type Store = Env Value

-- | Semantics of type-correct expressions.
evalExpr :: Expr -> Store -> Value
evalExpr (Lit i)    _ = I i
evalExpr (Add l r)  m = I (evalInt l m + evalInt r m)
evalExpr (Mul l r)  m = I (evalInt l m * evalInt r m)
evalExpr (LTE l r)  m = B (evalInt l m <= evalInt r m)
evalExpr (Not e)    m = B (not (evalBool e m))
evalExpr (And l r)  m = B (evalBool l m && evalBool r m)
evalExpr (Or  l r)  m = B (evalBool l m || evalBool r m)
evalExpr (Ref x)    m = case lookup x m of
                          Just v  -> v
                          Nothing -> error "internal error: undefined variable"
evalExpr (Nil _)    _ = L []
evalExpr (Cons h t) m = L (evalExpr h m : evalList t m)
evalExpr (Head e)   m = case evalList e m of
                          (h:_) -> h
                          []    -> error "language design error: applied head to an empty list :-("
evalExpr (Tail e)   m = case evalList e m of
                          (_:t) -> L t
                          []    -> error "language design error: applied tail to an empty list :-("
evalExpr (IsNil e)  m = case evalList e m of
                          (_:_) -> B False
                          []    -> B True


-- | Helper function to evaluate an expression to an integer.
evalInt :: Expr -> Store -> Int
evalInt e m = case evalExpr e m of
                I i -> i
                _   -> error "internal error: expected Int got something else"


-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Expr -> Store -> Bool
evalBool e m = case evalExpr e m of
                 B b -> b
                 _   -> error "internal error: expected Bool got something else"

-- | Helper function to evaluate an expression to a list.
evalList :: Expr -> Store -> [Value]
evalList e m = case evalExpr e m of
                 L l -> l
                 _   -> error "internal error: expected list got something else"


-- | Semantics of statements.
evalStmt :: Stmt -> Store -> Store
evalStmt (Bind x e)  m = insert x (evalExpr e m) m
evalStmt (If c t e)  m = if evalBool c m
                         then evalStmt t m
                         else evalStmt e m
evalStmt (While c b) m = if evalBool c m
                         then evalStmt (While c b) (evalStmt b m)
                         else m
evalStmt (Block ss)  m = evalStmts ss m


-- | Helper function to evaluate a list of statements.
evalStmts :: [Stmt] -> Store -> Store
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)


-- | Semantics of programs. Runs a program with an initial store where all
--   integer variables are initialized to 0, and all Boolean variables are
--   initialized to false.
evalProg :: Prog -> Store
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt      = I 0
    init TBool     = B False
    init (TList _) = L []

-- | Type check and then run a program.
runProg :: Prog -> Maybe Store
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing
