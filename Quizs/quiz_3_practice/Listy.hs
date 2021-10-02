--
-- * Listy language
--

-- ** Abstract syntax

-- | Expressions in Listy, a little language for manipulating lists.
data Expr
   = Lit Int         -- integer literal
   | One Expr        -- create a singleton list
   | Cons Expr Expr  -- prepend an element to a list
   | Sum Expr        -- sum of an integer list
   | Concat Expr     -- concatenate all lists in a list of lists
  deriving (Eq,Show)

-- | Encode a Haskell list of integers as a Listy list.
--   Assumes the Haskell list is non-empty.
list :: [Int] -> Expr
list [i]    = One (Lit i)
list (i:is) = Cons (Lit i) (list is)

-- Each of the following programs is type correct. Your type system should
-- return a type for each of them. You can run print out each program in GHCi
-- to see the full program and you call 'eval' to see what each evaluates to.
-- Seeing what a program evaluates to reveals what its type should be too.
good1 = list [1,2,3]
good2 = Sum good1
good3 = Cons good2 (list [2,3,4])
good4 = Sum good3
good5 = Cons good1 (Cons good3 (One (One (Lit 5))))
good6 = Concat good5
good7 = Sum good6

goods = [good1, good2, good3, good4, good5, good6, good7]

-- The following programs are all type incorrect. Your type system should
-- return 'Nothing' for all of these.
bad1 = Cons (Lit 1) (Lit 2)
bad2 = Cons (One (Lit 1)) (One (Lit 2))
bad3 = Cons (Lit 1) (One (One (Lit 2)))
bad4 = Sum (Lit 2)
bad5 = Concat (Lit 2)
bad6 = Sum good5
bad7 = Concat good3

bads = [bad1, bad2, bad3, bad4, bad5, bad6, bad7]


-- ** Type system

-- | Types.
data Type
   = TInt
   | TList Type
  deriving (Eq,Show)

-- | Typing relation.
typeOf :: Expr -> Maybe Type
typeOf (Lit _)      = Just TInt
typeOf (One e)      = case typeOf e of
                        Just t -> Just (TList t)
                        _ -> Nothing
typeOf (Cons e1 e2) = case (typeOf e1, typeOf e2) of
                        (Just t1, Just (TList t2)) -> if t1 == t2 then Just (TList t1) else Nothing
                        _ -> Nothing
typeOf (Sum e)      = case typeOf e of
                        Just (TList TInt) -> Just TInt
                        _ -> Nothing
typeOf (Concat e)   = case typeOf e of
                        Just (TList (TList t)) -> Just (TList t)
                        _ -> Nothing


-- ** Semantics

-- | Values of type correct expressions.
data Value = I Int | L [Value]
  deriving (Eq,Show)

-- | Run a list language program by first type checking it, then evaluating it
--   if it is type correct.
run :: Expr -> Maybe Value
run e = case typeOf e of
          Just _  -> Just (eval e)
          Nothing -> Nothing

-- | Semantic function that assumes the expression is type correct.
eval :: Expr -> Value
eval (Lit i)    = I i
eval (One e)    = L [eval e]
eval (Cons h t) = L (eval h : asList (eval t))
eval (Sum e)    = I (sum (map asInt (asList (eval e))))
eval (Concat e) = L (concat (map asList (asList (eval e))))

-- | Helper function to get a value as an integer. Should only be called on
--   values that are known to be ints (because of type checking).
asInt :: Value -> Int
asInt (I i) = i
asInt v     = error ("internal error: expected int, got: " ++ show v)

-- | Helper function to get a value as a list. Should be called on values that
--   are known to be lists (because of type checking.
asList :: Value -> [Value]
asList (L vs) = vs
asList v      = error ("internal error: expected list, got: " ++ show v)
