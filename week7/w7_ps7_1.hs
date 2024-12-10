import Data.Char (toLower, ord, isLetter)

-----Tail Recursion

badFib :: Int -> Int
badFib 1 = 1
badFib 2 = 1
badFib n = badFib (n-1) + badFib (n-2)

{-badFib 6 = badFib 5 + badFib 4
           = (badFib 4 + badFib 3) + (badFib 3 + badFib 2)
           = ((badFib 3 + badFib 2) + (badFib 2 + badFib 1)) + ((badFib 2 + badFib 1) + 1)
           = (((badFib 2 + badFib 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
           = (((1 + 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
           = 8

The number of calls of badFib is exponentional in n.
-}

fibAcc :: Int -> Integer -> Integer -> Integer
fibAcc 1 cur _ = cur
fibAcc n cur prev = fibAcc (n-1) (cur+prev) cur

goodFib :: Int -> Integer
goodFib n = fibAcc n 1 0

{-goodFib 8 = fibAcc 8 1 0
            = fibAcc 7 1 1
            = fibAcc 6 2 1
            = fibAcc 5 3 2
            = fibAcc 4 5 3
            = fibAcc 3 8 5
            = fibAcc 2 13 8
            = fibAcc 1 21 13
            = 21
-}

fibList :: [Integer]
fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

{-fibList = 1 : 1 :            zipWith zipWith (+) fibList (tail fibList)
          = 1 : 1 : 2 :        zipWith zipWith (+) fibList (tail fibList)
          = 1 : 1 : 2 : 3 :    zipWith zipWith (+) fibList (tail fibList)
          = 1 : 1 : 2 : 3 : 5: zipWith zipWith (+) fibList (tail fibList)
          = ...
-}

-----Hard Recursion Exercises

wordScore :: String -> Int
wordScore [] = 0
wordScore (x:xs) | isLetter x = subtract (ord 'a' - 1) (ord (toLower x)) + wordScore xs
                 | otherwise = wordScore xs

wordScore2 :: String -> Int
wordScore2 str = sum [subtract (ord 'a' - 1) (ord (toLower x)) | x <- str, isLetter x]

wordScore3 :: String -> Int
wordScore3 str = sum (map f str)
    where
        f x | isLetter x = subtract (ord 'a' - 1) (ord (toLower x))
            | otherwise = 0

wordScore4 :: String -> Int
wordScore4 str = sum (map (\x -> subtract (ord 'a' - 1) (ord (toLower x))) (filter isLetter str))

wordScore5 :: String -> Int
wordScore5 str = sum (map (subtract (ord 'a' - 1) . ord . toLower) (filter isLetter str))

wordScore6 :: String -> Int
wordScore6 = foldr f 0
    where
        f x acc | isLetter x = subtract (ord 'a' - 1) (ord (toLower x)) + acc
                | otherwise = acc

-- wordScore2 :: String -> Int

concatCheapWords :: [String] -> String
concatCheapWords [] = ""
concatCheapWords (x:xs) | wordScore x <= 42 = " " ++ x ++ concatCheapWords xs
                        | otherwise = concatCheapWords xs

concatCheapWords2 :: [String] -> String
concatCheapWords2 str = concat [" " ++ x | x <- str, wordScore x <= 42]


concatCheapWords3 :: [String] -> String
concatCheapWords3 = concatMap f
  where
    f x | wordScore x <= 42 = " " ++ x
        | otherwise         = ""

concatCheapWords4 :: [String] -> String
concatCheapWords4 str = concatMap (" " ++) (filter (\x -> wordScore x <= 42) str)
