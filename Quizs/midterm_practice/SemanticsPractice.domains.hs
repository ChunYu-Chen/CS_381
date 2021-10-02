-- Semantics practice problems.
module SemanticsPractice where


-- * Advanced integer arithmetic language

-- Below is the abstract syntax for an integer artithmetic language that
-- supports division. How exciting! Division may fail with an error if the
-- second argument evaluates to 0. The other operations work as you expect.

data Expr = Lit Int         -- integer literal
          | Neg Expr        -- integer negation
          | Add Expr Expr   -- integer addition
          | Mul Expr Expr   -- integer multiplication
          | Div Expr Expr   -- integer division
  deriving (Eq,Show)


-- What is a good choice of semantic domain for Expr?
--
--   Maybe Int

-- Define the semantic function for Expr. You should use the `div` function
-- from the Prelude for integer division.

-- | Semantic function.
--
--   >>> expr (Neg (Div (Mul (Lit 4) (Lit 6)) (Add (Lit 2) (Lit 3))))
--   Just (-4)
--
--   >>> expr (Div (Lit 10) (Add (Lit 5) (Neg (Lit 5))))
--   Nothing
--
expr :: Expr -> Maybe Int
expr (Lit x) = Just x
expr (Neg x) = case expr x of
                    Just a -> Just (negate a)
                    Nothing -> Nothing
expr (Add x y) = case (expr x, expr y) of
                    (Just a, Just b) -> Just (a+b)
                    _ -> Nothing
expr (Mul x y) = case (expr x, expr y) of
                    (Just a, Just b) -> Just (a*b)
                    _ -> Nothing
expr (Div x y) = case (expr x, expr y) of
                    (Just a, Just b) -> if b == 0 then Nothing else Just (a `div` b)
                    _ -> Nothing


-- * Simple counter language

-- Below is the abstract syntax for a simple language for manipulating an
-- integer counter. Statements either increment the counter by a given
-- integer, or they reset the counter to zero. A program runs a sequence of
-- statements on an initial counter of 0 and returns the final value of the
-- counter.

data Stmt = Inc Int   -- increment the counter
          | Reset     -- reset to zero
  deriving (Eq,Show)

type CProg = [Stmt]   -- counter programs


-- What is a good choice of semantic domain for Stmt?
--
--   Int -> Int
--
-- What is a good choice of semantic domain for CProg?
--
--   Int


-- Define the denotational semantics of the counter language by defining each
-- of the following semantic functions.

-- | Semantics of statements.
stmt :: Stmt -> Int -> Int
stmt = undefined

-- | Semantics of a sequence of statements. This should work for any initial
--   counter value (i.e. it should have the same semantic domain as 'stmt').
stmts :: [Stmt] -> Int -> Int
stmts = undefined

-- | Semantics of programs. This should run a sequence of statements on an
--   initial counter of 0.
--
--   >>> cprog [Inc 2, Inc 3]
--   5
--
--   >>> cprog [Inc 2, Inc 3, Reset, Inc 4, Inc 5]
--   9
--
cprog :: CProg -> Int
cprog = undefined



-- * Robot arm language

-- Below is the abstract syntax of a command language for controlling a robot
-- that moves in a one-dimensional space (i.e. back and forth along a line).

data Cmd = Gas | Brake | Turn
  deriving (Eq,Show)

type ArmProg = [Cmd]  


-- The *execution state* of the robot is represented by three components: its
-- current position on the line, its current speed, and its current direction.
-- Moving forward corresponds to increasing the position, while moving backward
-- decreases the position.

type Pos   = Int
type Speed = Int

data Dir = Forward | Backward
  deriving (Eq,Show)

-- | The state of the robot.
type State = (Pos, Speed, Dir)


-- The commands work as follows:
--
--  * Gas: Move in the current direction an amount equal to the current speed,
--    then increase the speed by one. For example, if the robot is at
--    position 5 while moving forward at a speed of 2, after executing a Gas
--    command the robot would be at position 7 moving at a speed of 3.
-- 
--  * Brake: Move in the current direction an amount equal to the current speed,
--    then decrease the speed by one down to a minimum speed of 0. If the robot
--    is already at speed 0, then a Brake command has no effect.
-- 
--  * Turn: If the current speed of the robot is 0, then change the direction
--    of the robot. If the speed is not 0, the robot crashes. If the robot
--    crashes, it no longer has a speed or direction, but it does still have a
--    position (the position it was at when it crashed).


-- What is a good choice of semantic domain for Cmd?
-- 
--   State -> Result   --or--   State -> Either State Pos
--
-- You should use the same semantic domain for ArmProg.

data Result = OK State
            | Crash Pos
  deriving (Eq,Show)


-- Define the denotational semantics of the robot arm language by defining each
-- of the following semantic functions.


-- | Semantics of commands.
cmd :: Cmd -> State -> Result
cmd = undefined


-- | Semantics of robot arm programs.
--
--   >>> arm [Gas, Gas, Gas, Brake] (0,0,Forward)
--   OK (6,2,Forward)
--
--   >>> arm [Gas, Brake, Gas, Brake] (0,0,Backward)
--   OK (-2,0,Backward)
--
--   >>> arm [Gas, Gas, Brake, Turn, Gas, Gas] (0,0,Forward)
--   Crash 3
--
--   >>> arm [Gas, Gas, Brake, Turn] (0,0,Forward)
--   Crash 3
--
--   >>> arm [Gas, Gas, Brake, Brake, Turn, Gas, Gas] (0,0,Forward)
--   OK (3,2,Backward)
--
arm :: ArmProg -> State -> Result
arm = undefined
