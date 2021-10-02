data Expr
   = LitI Int        -- literal integer value
   | LitS String     -- literal string value
   | Add  Expr Expr  -- add two integers, producing an integer
   | Cat  Expr Expr  -- concatenate two strings, producing a string 
   | Show Expr       -- convert an integer into a string

-- | Types.
data Type
   = TInt
   | TString
  deriving (Eq,Show)

typeOf :: Expr -> Maybe Type
typeOf (LitI _)      = Just TInt
typeOf (LitS _)      = Just TString
typeOf (Add ex1 ex2) = case (typeOf ex1, typeOf ex2) of
                         (Just t1,Just t2) -> if (t1 == TInt && t2 == TInt) then Just TInt else Nothing
                         _                 -> Nothing
typeOf (Cat ex1 ex2) = case (typeOf ex1, typeOf ex2) of
                         (Just t1,Just t2) -> if (t1 == TString && t2 == TString) then Just TString else Nothing
                         _                 -> Nothing
typeOf (Show ex1)    = case (typeOf ex1) of 
                         (Just TInt) -> Just TString
                         _           -> Nothing
