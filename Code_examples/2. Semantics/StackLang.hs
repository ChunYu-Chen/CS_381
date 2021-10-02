module StackLang where

import Prelude hiding (Num)


--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
-- 
--    int ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= int           push a number on the stack
--         |  bool          push a boolean on the stack
--         |  `+`           add the top two integers on the stack
--         |  `*`           multiply the top two integers on the stack
--         |  `<=`          is the top integer LEQ the second integer on the stack
--         |  `if`   prog   if the value on the top is true, then run
--            `else` prog   the first program, else run the second
--            `end`

-- Examples of real world stack-based languages:
--  * Forth
--  * Postscript
--  * HP programmable calculators
--  * Java Virtual Machine


-- 1. Encode the above grammar as a set of Haskell data types

type Prog = [Cmd]

data Cmd
   = PushI Int
   | PushB Bool
   | Add
   | Mul
   | LEq
   | IfElse Prog Prog
  deriving (Eq,Show)


-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 + 5 <=
--
ex1 :: Prog
ex1 = [PushI 3, PushI 4, Add, PushI 5, LEq]


-- 3. Write a StackLang program that:
--     * checks whether 3 is less than or equal to 4
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 <= if 5 6 + else false end
--
ex2 :: Prog
ex2 = [PushI 3, PushI 4, LEq, IfElse [PushI 5, PushI 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack.
genAdd2 :: Int -> Int -> Prog
genAdd2 x y = [PushI x, Add, PushI y, Add]
-- genAdd2 x y = [PushI x, PushI y, Add, Add]


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum []     = [PushI 0]
genSum (i:is) = genSum is ++ [PushI i, Add]

-- This also works, but our stack gets big!
--
-- genSum (i:is) = PushI i : genSum is ++ [Add]


--
-- * Semantics of StackLang (now!)
--


-- 6. Identify/define a semantics domain for Cmd and for Prog.

-- Concepts that we need for our semantics
--  * We need a Stack with these kinds of elements
--    * Int
--    * Bool
--  * Our stack will change as we execute commands
--    * Only things commands do is change the stack: Stack -> Stack
--  * Errors:
--    * type errors
--    * not enough things on the stack

type Stack = [Either Bool Int]

type Domain = Stack -> Maybe Stack

-- 7. Define the semantics of a StackLang command (ignore if-else at first).
cmd :: Cmd -> Stack -> Maybe Stack
cmd (PushI i)    s                       = Just (Right i : s)
cmd (PushB b)    s                       = Just (Left b : s)
cmd Add          (Right i : Right j : s) = Just (Right (i + j) : s)
cmd Mul          (Right i : Right j : s) = Just (Right (i * j) : s)
cmd LEq          (Right i : Right j : s) = Just (Left (j <= i) : s)
cmd (IfElse t _) (Left True : s)         = prog t s
cmd (IfElse _ e) (Left False : s)        = prog e s
cmd _            _                       = Nothing


-- 8. Define the semantics of a StackLang program.
prog :: Prog -> Stack -> Maybe Stack
prog []     s = Just s
prog (c:cs) s = case cmd c s of
                  Just s' -> prog cs s'
                  Nothing -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right 11]
--
--   >>> run (genSum [1..10])
--   Just [Right 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
run = undefined
try :: Int -> Int -> [Int]
try a b = [a:b]