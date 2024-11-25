
type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--   deriving Show

instance Show Term where
 show = pretty


example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1

numeral :: Int -> Term
numeral i | i < 0 = error "Church numerals only handle non-negative integers, including zero."
          | i == 0 =  Lambda "f" (Lambda "x" (Variable "x"))
          | i > 0  = Lambda "f" (Lambda "x" (f i (Variable "x")))
            where 
                f 0 term = term
                f i term = Apply (Variable "f") (f (i-1) term)

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------- Assignment 2

variables :: [Var]
variables = [ c : i | i <- "" : map show [1..], c <- ['a'..'z']]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables a b = filter (`notElem` b) a

fresh :: [Var] -> Var
fresh l = head (filterVariables variables l)

used :: Term -> [Var]
used (Variable v) = [v]
used (Lambda v t) = merge [v] (used t)
used (Apply t0 t1) = merge (used t0) (used t1) 


------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z) | z == x = Variable y
                        | otherwise = Variable z
rename x y (Lambda z n) | z == x = Lambda z n
                        | otherwise = Lambda z (rename x y n)
rename x y (Apply  n m) = Apply (rename x y n) (rename x y m)  


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y) | y == x = n
                            | otherwise = Variable y
substitute x n (Lambda y m) | y == x = Lambda y m
                            | otherwise = Lambda fv (substitute x n (rename y fv m))
                                where fv = fresh (merge [x] (merge (used m) (used n)))
substitute x n (Apply m1 m2) = Apply (substitute x n m1) (substitute x n m2)  


------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda x y) t) = [substitute x t y] ++ [Apply n t | n <- beta (Lambda x y)] ++ [Apply (Lambda x y) n | n <- beta t]
beta (Variable x) = [] 
beta (Lambda x y) = [Lambda x t | t <- beta y]
beta (Apply n m) = [Apply n' m | n' <- beta n] ++ [Apply n m' | m' <- beta m]

normalize :: Term -> [Term]
normalize term | null (beta term) = [term]
               | otherwise = term : normalize(head (beta term))  

normal :: Term -> Term
normal term | null (beta term) = term
            | otherwise = normal (head (beta term))

-- i) The functions beta, normalize and normal implement the normal-order reduction strategy.
--    "normalize" and "normal" use "beta" to perform the computations. "beta" performs all 
--    the possible reductions but the first reduction of the output list corresponds to the
--    normal-order reduction. This can be tested with the following simple
--    example:
simple_example :: Term
simple_example = Apply (Lambda "x" (Variable "x")) (Apply (Lambda "y" (Variable "y")) (Variable "z"))
--    This more complex example also validates that "beta" returns the call-by-name reduction
--    in the first position of the list:
complex_example :: Term
complex_example = Apply (Lambda "x" (Apply (Variable "a") (Variable "x"))) (Apply (Lambda "y" (Apply (Variable "b") (Variable "y"))) (Apply (Lambda "z" (Apply (Variable "c") (Variable "z"))) (Variable "d")))

------------------------- 
-- Applicative-order reduction
a_beta :: Term -> [Term]
a_beta (Apply (Lambda x y) t) = [Apply (Lambda x y) n | n <- a_beta t] ++ [Apply n t | n <- a_beta (Lambda x y)] ++ [substitute x t y]
a_beta (Variable x) = [] 
a_beta (Lambda x y) = [Lambda x t | t <- a_beta y]
a_beta (Apply n m) =  [Apply n' m | n' <- a_beta n] ++ [Apply n m' | m' <- a_beta m]

a_normalize :: Term -> [Term]
a_normalize term | null (a_beta term) = [term]
                 | otherwise = term : a_normalize(head (a_beta term))  

a_normal :: Term -> Term
a_normal term | null (a_beta term) = term
              | otherwise = a_normal (head (a_beta term))

-------------------------

example1 :: Term
example1 = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Apply (Lambda "y" (Variable "y")) (Variable "w"))

example2 :: Term
example2 = Apply (Lambda "x" (Variable "y")) (Apply (Lambda "z" (Variable "z")) (Variable "w"))

-- example3 was extracted from the course materials to test the normal-order reduction and applicative-order reduction functions.
example3 :: Term
example3 = Apply (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Variable "f") (Variable "x"))))) (Lambda "a" (Apply (Lambda "x" (Variable "x")) (Apply (Lambda "y" (Variable "y")) (Apply (Lambda "z" (Variable "z")) (Variable "a")))))
