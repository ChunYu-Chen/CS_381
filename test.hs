module Test where

-- | This function doesn’t terminate.
loop :: Int -> Int
loop x = x + 999

-- | This function takes one minute to return a value.
slow :: Int -> Int
slow x = 1

-- | This is the function you have to think about.
go :: Bool -> Int -> Int -> Int
go b x y = if b then x else y + y + y