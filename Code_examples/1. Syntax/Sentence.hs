module Sentence where

-- Grammar for the animal sentence language:
--
--   s  ::=  n v n  |  s `and` s
--   n  ::=  `cats` | `dogs` | `ducks`
--   v  ::=  `chase` | `cuddle`

data Sentence
   = NVN Noun Verb Noun
   | And Sentence Sentence
  deriving (Eq,Show)

data Noun = Cats | Dogs | Ducks
  deriving (Eq,Show)

data Verb = Chase | Cuddle
  deriving (Eq,Show)


-- | The sentence: cats cuddle ducks and dogs cuddle ducks
ex1 :: Sentence
ex1 = And (NVN Cats Cuddle Ducks) (NVN Dogs Cuddle Ducks)

-- | The sentence: dogs chase cats and cats chase ducks and ducks chase cats
ex2 :: Sentence
ex2 = And (NVN Dogs Chase Cats) (And (NVN Cats Chase Ducks) (NVN Ducks Chase Cats))
-- ex2 = And (And (NVN Dogs Chase Cats) (NVN Cats Chase Ducks)) (NVN Ducks Chase Cats)

-- | Build a sentence that is a conjunction of other sentences.
conjunction :: [Sentence] -> Maybe Sentence
conjunction []    = Nothing
conjunction (h:t) = case conjunction t of
                      Nothing -> Just h
                      Just s  -> Just (And h s)

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs  = "dogs"
prettyNoun Ducks = "ducks"

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle = "cuddle"

-- | Does the sentence contain only cuddling?
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)  = False
isNice (NVN _ Cuddle _) = True
isNice (And l r)        = isNice l && isNice r
