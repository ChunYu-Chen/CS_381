module Basics where


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :quit, :load, :reload, :type, :info, :help
-- * Hoogle
-- * doctest



---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions



---------------------
-- Basic Functions --
---------------------

-- * function types
-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = x + x


-- | Is this integer zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- OR:
-- isZero x = x == 0


-- | Is this integer non-zero?
isNonZero :: Int -> Bool
isNonZero = not . isZero

-- OR:
-- isNonZero x = not (isZero x)

-- OR:
-- isNonZero 0 = False
-- isNonZero _ = True

-- OR: 
-- isNonZero x = x /= 0


-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg x y = (x + y) / 2.0


-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
half = avg 0

-- OR:
-- half x = avg 0 x


-- | An operator version of avg.
(%%) :: Float -> Float -> Float
(%%) = avg


-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions



----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions


-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)


-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv x 0 = Error
safeDiv x y = OK (x `div` y)


-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
fromResult x = case x of
                 Error -> 0
                 OK y  -> y

-- OR:
-- fromResult Error  = 0
-- fromResult (OK x) = x


-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK x) (OK y) = OK (x + y)
addResults _      _      = Error

-- OR:
-- addResults l r = case l of
--                    OK x -> case r of
--                              OK y -> OK (x+y)
--                              _    -> Error
--                    _ -> Error

-- OR:
-- addResults l r = case (l, r) of
--                    (OK x, OK y) -> OK (x+y)
--                    _            -> Error


-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
--   data Maybe a = Just a | Nothing



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
empty :: List
empty = Nil

-- | The list: [2,3,4]
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 Nil))

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil        = 0
listLength (Cons _ t) = 1 + listLength t

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum Nil        = 0
listSum (Cons h t) = h + listSum t


-- Example evaluation:
--
--    listSum (Cons 3 (Cons 4 Nil))
-- => 3 + listSum (Cons 4 Nil)
-- => 3 + (4 + listSum Nil)
-- => 3 + (4 + 0)
-- =>* 7
