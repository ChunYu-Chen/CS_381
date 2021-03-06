-- Group members:
-- Chun-Yu Chen, 934-148-121
--
-- Grading notes: 10pts total
--  * 2pts expr
--  * 3pts cmd
--  * 3pts block
--  * 2pts optimize
--
-- Supporting files:
--
--  * MiniMiniLogo.hs -- Defines the syntax of MiniMiniLogo, pretty printing
--    functions, and several types that you'll need to implement the semantics.
--    Also includes functions to generate example MiniMiniLogo programs.
--
--  * Render.hs -- Contains code for rendering the semantics of Mini(Mini)Logo
--    programs in HTML5.
--
module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- | The state of the pen, which includes whether it is up or down and its
--   current location.
type State = (Mode, Point)

-- | The initial state of the pen.
initPen :: State
initPen = (Up, (0,0))


-- | A function that uses the semantic functions that you will implement
--   to render an image. Once your semantic functions are working, you should
--   be able to apply `draw` to a MiniMiniLogo program, then load the file
--   MiniLogo.html in your browswer to view the rendered image.
draw :: Prog -> IO ()
draw = toHTML . prog


-- Semantic domains:
--   * Expr: Int
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantics of expressions.
--
--   >>> expr (Add (Mul (Lit 2) (Lit 3)) (Mul (Lit 4) (Lit 5)))
--   26
-- 
--   >>> expr (Mul (Add (Lit 2) (Lit 3)) (Add (Lit 4) (Lit 5)))
--   45
--
expr :: Expr -> Int
expr (Lit x) = x
expr (Add a b) = expr a + expr b
expr (Mul a b) = expr a * expr b


-- | Semantics of commands. Updates the pen state and possibly returns a line.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move (Lit 4) (Lit 5)) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move (Lit 4) (Lit 5)) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen mode) (_, t) = ((mode, t), Nothing)
cmd (Move x y) (s, t) = case s of
                            Up   -> ((Up, (expr x, expr y)), Nothing)
                            Down -> ((Down, (expr x, expr y)), Just(t,(expr x, expr y)))


-- | Semantics of blocks. Evaluates each command in sequence and accumulates
--   any lines produced.
--
--   >>> block (genBox 1 2 3 4) initPen
--   ((Down,(1,2)),[((1,2),(4,2)),((4,2),(4,6)),((4,6),(1,6)),((1,6),(1,2))])
--
--   >>> block (genSteps 2 0 0) initPen
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
block :: Prog -> State -> (State, [Line])
block [] s = (s, [])
block (h:t) s = case cmd h s of
                   (a, Nothing) -> block t a
                   (b, Just c) -> ( calState(block t b),[c]++ calPline(block t b))

calState :: (State,[Line]) -> State
calState (s,_) = s

calPline :: (State,[Line]) -> [Line]
calPline (_,l) = l 

-- | Semantics of programs. Evaluates the main block with the initial pen
--   state and returns any lines produced.
prog :: Prog -> [Line]
prog p = snd (block p initPen)


-- | Optimize a MiniMiniLogo program by evaluating all of the expressions to
--   literal integers.
--   
--   >>> optimize [Move (Add (Lit 2) (Lit 3)) (Mul (Lit 4) (Lit 5))]
--   [Move (Lit 5) (Lit 20)]
--
--   >>> putStrLn (pretty (optimize (genSteps 3 4 2)))
--   main() {
--     pen up;
--     move(4, 2);
--     pen down;
--     move(4, 3);
--     move(5, 3);
--     move(5, 4);
--     move(6, 4);
--     move(6, 5);
--     move(7, 5)
--   }
--
optimize :: Prog -> Prog
optimize [] = []
optimize (h:t) = case h of 
                    (Move x y) -> [Move (Lit (expr x)) (Lit (expr y))] ++ optimize t
                    (Pen mode) -> [Pen mode] ++ optimize t
