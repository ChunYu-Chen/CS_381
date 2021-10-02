data  Reg = A | B deriving (Eq,Show)

data Exp = Lit Int | Load Reg | Add Exp Exp deriving (Eq,Show)

data Stmt = Set Reg Exp | Swap | Ifelse Stmt Stmt deriving (Eq,Show)

type Prog = [Stmt]


example :: Prog
example = [Set A (Lit 2), Set B (Add (Load A) (Lit 3)), Ifelse Swap (Set B (Add (Load A) (Load B)))]

expr :: Exp
expr (Lit x) = x
expr (Load A) = A
expr (Load B) = B
--expr (Add x y) = expr x + expr y
