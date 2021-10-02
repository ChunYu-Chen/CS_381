-- | A truly exceptional imperative programming language with:
--    * three registers: A, B, and R
--    * conditional statements
--    * unconditional loops + break statements
--    * exceptions + try-catch
module Exceptional where


-- Goals:
--
--  * More discussion of "syntactic sugar" and how it supports good language
--    design
--
--  * How to implement structured jumps (e.g. break, try-catch)
--
--  * More practice at the most important concepts from the class (abstract
--    syntax, semantcs) to review for final exam


--
-- * Syntax
--


-- ** Abstract syntax

-- | Our registers.
data Reg = A | B | R
  deriving (Eq, Show)

-- | Integer expressions.
data Expr
   = Get Reg             -- load from register
   | Lit Int             -- integer literal
   | Add Expr Expr       -- integer addition
   | Mul Expr Expr       -- integer multiplication
  deriving (Eq,Show)

-- | Boolean conditions.
data Test
   = Not Test            -- boolean negation
   | And Test Test       -- boolean conjunction
   | Or  Test Test       -- boolean disjunction
   | LTE Expr Expr       -- less than or equal
  deriving (Eq,Show)

-- | Statements.
data Stmt
   = Set Reg Expr        -- set register
   | Block [Stmt]        -- statement block
   | If Test Stmt Stmt   -- conditional statement
   | Loop Stmt           -- unconditional loop
   | Break               -- break statement
   | Try Stmt Stmt       -- try-catch statement
   | Throw String        -- throw an exception
  deriving (Eq,Show)


-- ** Syntactic sugar

-- Orthogonality: a design principle for programming languages. Each construct
-- in the *abstract syntax* of your language should be:
--
--   1. Unique: it shouldn't do the same thing as another construct.
--
--   2. Composable: it should stand on its own and be usable with any other
--      construct.
--
-- Rationale: maximize expressiveness while minimizing complexity (of the
-- language itself and its implementation).
--
-- However, redundant constructs for slightly different uses are convenient for
-- programmers! For example, many languages have several looping constructs:
-- while, do-while, for, for-each, ...
--
-- Solution: syntactic sugar!
--
--   * A construct that exists in the concrete syntax but not the abstract
--     syntax.
--
--   * Expanded into the abstract syntax early in the compiler (possibly 
--     right after parsing, but often after type checking to support better error messages).
--     sugar into the core abstract syntax.


-- *** Expression sugar

-- | Integer negation.
neg :: Expr -> Expr
neg = Mul (Lit (-1))

-- | Integer subtraction.
sub :: Expr -> Expr -> Expr
sub l r = Add l (neg r)

-- | Comparison operations.
eq, neq, lt, gt, gte :: Expr -> Expr -> Test
eq  l r = And (LTE l r) (LTE r l)
neq l r = Not (eq l r)
lt  l r = And (LTE l r) (Not (eq l r))
gt  l r = Not (LTE l r)
gte l r = Or  (gt l r) (eq l r)


-- *** Statement sugar

-- | A no-op statement.
nop :: Stmt
nop = Block []

-- | Syntactic sugar for an if-then statement.
ifThen :: Test -> Stmt -> Stmt
ifThen cond sthen = If cond sthen nop

-- | Syntactic sugar for an unless statement.
unless :: Test -> Stmt -> Stmt
unless cond selse = If cond nop selse

-- | Syntactic sugar for a while-loop.
while :: Test -> Stmt -> Stmt
while cond body = Loop (Block [unless cond Break, body])

-- | Syntactic sugar for a C-style for-loop.
for :: Stmt -> Test -> Stmt -> Stmt -> Stmt
for init cond inc body = Block [init, while cond (Block [body, inc])]


-- ** Examples


-- | Sum every third number from 1 to the number in register B.
--
--     R := 0
--     for A := 1, A <= B, A := A + 3:
--       R := R + A
--
exFor :: Stmt
exFor =
    Block [
      Set R (Lit 0),
      for (Set A (Lit 1))
          (LTE (Get A) (Get B))
          (Set A (Add (Get A) (Lit 3)))
          (Set R (Add (Get R) (Get A)))
    ]


-- | Computes the greatest common divisor of A and B. Try with 306 and 657.
--
--     while A != B:
--       while A > B:
--         R = A - B
--         A = R
--       while B > A:
--         R = B - A
--         B = C
--   
exGCD :: Stmt
exGCD =
  while (neq (Get A) (Get B)) $ Block [
    while (gt (Get A) (Get B)) $ Block [
      Set R (sub (Get A) (Get B)),
      Set A (Get R)
    ],
    while (gt (Get B) (Get A)) $ Block [
      Set R (sub (Get B) (Get A)),
      Set B (Get R)
    ]
  ]


-- | Example illustrating uncaught exception
--
--   R := 1
--   loop:
--     R := R + 1
--     if R > 5:
--       throw "BOOM!"
--   R := R + 100
--
exError :: Stmt
exError =
  Block [
    Set R (Lit 1),
    Loop $ Block [
      Set R (Add (Get R) (Lit 1)),
      ifThen (gt (Get R) (Lit 5))
        (Throw "BOOM!")
    ],
    Set R (Add (Get R) (Lit 100))
  ]


-- | Example program using try-catch block:
-- 
--     R := 1
--     try
--       R := R + 2
--       throw "BOOM!"
--       R := R + 100
--     catch
--       R := R + 3
--       
exCatch :: Stmt
exCatch =
  Block [
    Set R (Lit 1),
    Try (Block [
      Set R (Add (Get R) (Lit 2)),
      Throw "BOOM!",
      Set R (Add (Get R) (Lit 100))
    ]) (Set R (Add (Get R) (Lit 3)))
  ]


--
-- * Semantics
--

-- | State of the three registers.
type State = (Int, Int, Int)

-- | Get the value of the indicated register from the state.
get :: Reg -> State -> Int
get A (a,_,_) = a
get B (_,b,_) = b
get R (_,_,r) = r

-- | Set the value of the indicated register in the state.
set :: Reg -> Int -> State -> State
set A i (_,b,r) = (i,b,r)
set B i (a,_,r) = (a,i,r)
set R i (a,b,_) = (a,b,i)

-- | Semantics of ingeger expressions.
expr :: Expr -> State -> Int
expr (Get r)     rs = get r rs
expr (Lit i)     _  = i
expr (Add e1 e2) rs = expr e1 rs + expr e2 rs
expr (Mul e1 e2) rs = expr e1 rs * expr e2 rs

-- | Semantics of boolean conditions.
test :: Test -> State -> Bool
test (Not t)     rs = not (test t rs)
test (And e1 e2) rs = test e1 rs && test e2 rs
test (Or  e1 e2) rs = test e1 rs || test e2 rs
test (LTE e1 e2) rs = expr e1 rs <= expr e2 rs

-- | Control flow mode.
data Mode = OK | BREAK | EXCEPT String
  deriving (Eq,Show)

-- | Semantics of statements.
stmt :: Stmt -> State -> (State, Mode)
stmt (Set r e)   rs = (set r (expr e rs) rs, OK)
stmt (Block ss)  rs = stmts ss rs
stmt (If c t e)  rs = if test c rs then stmt t rs else stmt e rs
stmt Break       rs = undefined
stmt (Throw msg) rs = undefined
stmt (Try s c)   rs = undefined
stmt (Loop s)    rs = undefined

-- | Execute a list of statements in sequence.
stmts :: [Stmt] -> State -> (State, Mode)
stmts []     rs = (rs, OK)
stmts (s:ss) rs = case stmt s rs of
                    (rs', OK) -> stmts ss rs'
                    breakOrExcept -> breakOrExcept
