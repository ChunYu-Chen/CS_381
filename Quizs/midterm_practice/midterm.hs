module Midterm where

data Verb
    = Eat 
    | Exist
    deriving(Eq, Show)

data Adjective 
    = Calla Adjective Adjective
    | Big
    | Tasty
    deriving(Eq, Show)

data Noun 
    = Calln Adjective Noun
    | Add Noun Noun
    | People
    | Avocados
    deriving(Eq, Show)

data Sentence 
    = Calls Noun Verb
    | Calls2 Noun Verb Noun
    | Ifthen Sentence Sentence
    deriving(Eq, Show)

ex :: Sentence 
ex = Ifthen (Calls People Exist) (Calls2 People Eat (Calln Big (Calln Tasty Avocados)))

type Prog = [Cmd]

data Cmd 
    = Grab 
    | Release 
    | Reach Int
    deriving(Eq, Show)

data Claw 
    = Open 
    | Closed  -- state of the claw, either open or closed
    deriving(Eq, Show)

type Arm = (Int, Claw) 
         -- state of the arm: arm position and claw state

cmd :: Cmd -> Arm -> Maybe Arm
cmd Grab    (x, Open)   = Just (x, Closed)
cmd Grab    (x, Closed) = Nothing
cmd Release (x, Closed) = Just (x, Open)
cmd Release (x, Open)   = Nothing
cmd (Reach a) (x, claw) = if (x+a) >= 0 then Just (x+a, claw) else Nothing

prog :: Prog -> Arm -> Maybe Arm
prog []     x = Just x
prog (x:xs) y = case cmd x y of
                 Just a -> prog xs a
                 Nothing -> Nothing