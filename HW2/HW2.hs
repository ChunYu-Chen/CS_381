-- Group members:
--Chun-Yu Chen, 934-148-121

-- Grading note: 10pts total
--  * 2pts each for encodeList and mapTree
--  * 3pts each for valueAt and pathTo
module HW2 where
import Control.Applicative

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
encodeList :: [a] -> Tree a
encodeList []    = End
encodeList (h:t) = Node h End (encodeList t)
--(Second way)
--encodeList = foldr (\h t -> Node h End t) End 



-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ End          = End
mapTree f (Node l m r) = Node (f l) (mapTree f m) (mapTree f r)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End            = Nothing
valueAt [] (Node ro l r) = Just ro
valueAt (h:t) (Node ro l r) = if h == R then valueAt t r
                             else valueAt t l




-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo x End = Nothing
--pathTo x (Node ro l r) | x == ro  = Just []
--                       | otherwise = case (pathTo x l) of
--                          Nothing -> case (pathTo x r) of
--                            Nothing -> Nothing
--                            Just g -> Just ([R]++g) 
--                          Just g -> Just([L]++g)
pathTo x (Node ro l r) | x == ro    = Just []
                      | otherwise = (fmap (L:) (pathTo x l)) <|> (fmap (R:) (pathTo x r))                       