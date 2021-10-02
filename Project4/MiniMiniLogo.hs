-- | This module defines the syntax of MiniMiniLogo and provides some
--   definitions that are needed to define its semantics. It also includes a
--   pretty printer and several examples, which are useful for debugging.
--
--   NOTE: You should not change the definitions in this file!
--
module MiniMiniLogo where

import Data.List (intercalate)


--
-- * Syntax
--

-- | A sequence of commands.
type Block = [Cmd]

-- | The mode of the pen.
data Mode = Down | Up
  deriving (Eq,Show)

-- | Expressions.
data Expr
   = Lit Int
   | Add Expr Expr
   | Mul Expr Expr
  deriving (Eq,Show)

-- | Commands.
data Cmd
   = Pen Mode
   | Move Expr Expr
  deriving (Eq,Show)

-- | A MiniMiniLogo program is just the body of the main macro.
type Prog = Block


--
-- * Pretty printing
--

-- These are helpful for debugging your MiniMiniLogo programs.

-- | Pretty print the pen mode.
prettyMode :: Mode -> String
prettyMode Down = "down"
prettyMode Up   = "up"

-- | Pretty print an expression.
prettyExpr :: Expr -> String
prettyExpr (Lit i)   = show i
prettyExpr (Add l r) = prettyExpr l ++ " + " ++ prettyExpr r
prettyExpr (Mul l r) = prettyHelp l ++ " * " ++ prettyHelp r
  where
    prettyHelp e@(Add _ _) = "(" ++ prettyExpr e ++ ")"
    prettyHelp e           = prettyExpr e

-- | Pretty print a command.
prettyCmd :: Cmd -> String
prettyCmd (Pen m)       = "pen " ++ prettyMode m
prettyCmd (Move l r)    = concat ["move(", prettyExpr l, ", ", prettyExpr r, ")"]

-- | Pretty print a block of commands.
prettyBlock :: Block -> String
prettyBlock [] = "{}"  -- special case for empty blocks
prettyBlock cs = "{\n  " ++ indent (prettyCmds cs) ++ "\n}"
  where
    indent = concatMap (\c -> if c == '\n' then "\n  " else [c])
    prettyCmds = intercalate ";\n" . map prettyCmd

-- | Pretty print a program.
pretty :: Prog -> String
pretty b = "main() " ++ prettyBlock b


--
-- * Generate example MiniMiniLo+go programs
--

-- | Generate a block that draws a single line between two points.
genLine :: Int -> Int -> Int -> Int -> Block
genLine x1 y1 x2 y2 =
    [ Pen Up
    , Move (Lit x1) (Lit y1)
    , Pen Down 
    , Move (Lit x2) (Lit y2)
    ]


-- | A friendly program.
--   
--   >>> putStrLn (prettyBlock hi)
--   {
--     pen up;
--     move(39, 22);
--     pen down;
--     move(39, 20);
--     pen up;
--     move(40, 22);
--     pen down;
--     move(40, 20);
--     pen up;
--     move(39, 21);
--     pen down;
--     move(40, 21);
--     pen up;
--     move(41, 22);
--     pen down;
--     move(41, 20)
--   }
hi :: Block
hi = genLine 39 22 39 20 ++ genLine 40 22 40 20
  ++ genLine 39 21 40 21 ++ genLine 41 22 41 20


-- | Generate a block that draws a box of width w and height h, whose
--   bottom left corner is at (x,y).
--
--   >>> putStrLn (prettyBlock (genBox 1 2 3 4))
--   {
--     pen up;
--     move(1, 2);
--     pen down;
--     move(1 + 3, 2);
--     move(1 + 3, 2 + 4);
--     move(1, 2 + 4);
--     move(1, 2)
--   }
--
genBox :: Int -> Int -> Int -> Int -> Block
genBox x y w h =
    [ Pen Up
    , Move (Lit x) (Lit y)
    , Pen Down
    , Move (Add (Lit x) (Lit w)) (Lit y)
    , Move (Add (Lit x) (Lit w)) (Add (Lit y) (Lit h))
    , Move (Lit x) (Add (Lit y) (Lit h))
    , Move (Lit x) (Lit y)
    ]


-- | Generate a block that draws n steps starting from point (x,y).
--
--   >>> putStrLn (prettyBlock (genSteps 3 4 2))
--   {
--     pen up;
--     move(4, 2);
--     pen down;
--     move(4 + 1 + -1, 2 + 1);
--     move(4 + 1, 2 + 1);
--     move(4 + 2 + -1, 2 + 2);
--     move(4 + 2, 2 + 2);
--     move(4 + 3 + -1, 2 + 3);
--     move(4 + 3, 2 + 3)
--   }
-- 
genSteps :: Int -> Int -> Int -> Prog
genSteps n x y =
    [ Pen Up
    , Move (Lit x) (Lit y)
    , Pen Down
    ] ++ step n
  where 
    step 0 = []
    step n = step (n-1) ++ 
      [ Move (Add (Add (Lit x) (Lit n)) (Lit (-1))) (Add (Lit y) (Lit n))
      , Move (Add (Lit x) (Lit n)) (Add (Lit y) (Lit n))
      ]


-- | A MiniMini logo program with a lot going on.
shebang :: Prog
shebang = hi
       ++ genBox 36 17 8 8
       ++ genSteps 36 2 2 ++ genSteps 18 2 20
       ++ genSteps 36 40 2 ++ genSteps 18 60 2
       ++ genBox 25 8 3 3 ++ genBox 52 31 3 3
