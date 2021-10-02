module Lists where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [] a         -- [Int], [Bool], [[Int]]
--    = []
--    | (:) a [a]    -- (x:xs)
--
-- This is equivalent to:
--
-- data List a
--    = Nil
--    | Cons a (List a)

-- When building lists, you can write elements separated by commas:
--
--    [1,2,3,4]
--
-- This is equivalent to:
--
--    1:(2:(3:(4:[])))

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]



-- | Compute the length of a list.
--
--   >>> length []
--   0
--
--   >>> length [1,2,3]
--   3
--
--   >>> length [True,True]
--   2
--
length :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t

-- | Compute the sum of an integer list.
--
--   >>> sum []
--   0
--
--   >>> sum [1,2,3]
--   6
--
sum :: [Int] -> Int
sum []    = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in an integer list.
--
--   >>> product []
--   1
--
--   >>> product [2,3,4]
--   24
--
product :: [Int] -> Int
product []    = 1
product (h:t) = h * product t

-- Suppose we want the product of an empty list to be 0.
productZero :: [Int] -> Int
productZero []     = 0
productZero (h:[]) = h
productZero (h:t)  = h * productZero t

-- productZero (Cons h Nil)

-- | Are all of the integers in this list odd?
--
--   >>> allOdd [3,5,7]
--   True
--
--   >>> allOdd [3,4,5]
--   False
--
--   >>> allOdd []
--   True
--
allOdd :: [Int] -> Bool
allOdd []    = True
allOdd (h:t) = odd h && allOdd t 

-- | Double all the elements in an integer list.
--   
--   >>> doubleAll []
--   []
--
--   >>> doubleAll [1,2,3]
--   [2,4,6]
--
doubleAll :: [Int] -> [Int]
doubleAll []    = []
doubleAll (h:t) = 2 * h : doubleAll t

-- | Flip all of the boolean values in a boolean list.
--
--   >>> notAll []
--   []
--
--   >>> notAll [True,False,True]
--   [False,True,False]
--
notAll :: [Bool] -> [Bool]
notAll []    = []
notAll (h:t) = not h : notAll t

-- | Apply the even function to all elements in the list.
--
--   >>> evenAll []
--   []
--
--   >>> evenAll [2,3,4]
--   [True,False,True]
--
evenAll :: [Int] -> [Bool]
evenAll []    = []
evenAll (h:t) = even h : evenAll t


----------------------------
-- Higher-Order Functions --
----------------------------


-- * Map

-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map _ []    = []
map f (h:t) = f h : map f t

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not
-- OR:
-- notAll' bs = map not bs

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = map even

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (2 *)
-- OR: 
-- doubleAll' = map (\x -> 2 * x)


-- * Fold

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x []    = x
foldr f x (h:t) = f h (foldr f x t)

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\_ -> (1 +)) 0
-- OR:
-- length' = foldr (\_ l -> 1 + l) 0

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = foldr (\i b -> odd i && b) True

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = foldr (\b c -> if b then c + 1 else c) 0
