import Data.Char

---Exercise 1

toUpperSt :: String -> String
toUpperSt [] = []
toUpperSt (c:cs) = toUpper c : toUpperSt cs

deleteDigits :: String -> String
deleteDigits [] = []
deleteDigits (x:xs) 
    | isDigit x = deleteDigits xs
    | otherwise = x : deleteDigits xs

leetSpeak :: String -> String
leetSpeak [] = "!"
leetSpeak (x:xs) = leetChar x : leetSpeak xs
  where
    leetChar 'e' = '7'
    leetChar 'o' = '0'
    leetChar 's' = 'z'
    leetChar c   = c

--Exercise 2

factors2 :: Int -> [Int]
factors2 0 = []
factors2 n | (n `mod` 2 == 0) =  2 : factors2 (n `div` 2)
           | otherwise        = [n]

factorsm :: Int -> Int -> [Int]
factorsm m 0 = []
factorsm m n | (n `mod` m == 0) =  m : factorsm m (n `div` m)
             | otherwise        = [n]

factorsFrom :: Int -> Int -> [Int]
factorsFrom _ 0 = []
factorsFrom m n | (n `mod` m == 0) = m : factorsFrom m (n `div` m)
                | m >= n = [n]
                | otherwise        = factorsFrom (m + 1) n

primeFactors :: Int -> [Int]
primeFactors n | n <= 1    = []
               | otherwise = factorsFrom 2 n

-- primeFactors :: Int -> [Int]
-- primeFactors = factorsFrom 2